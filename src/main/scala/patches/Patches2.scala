package patches

import com.softwaremill.quicklens.*

import scala.util.control.*
import scala.collection.*
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import schema.v1.{ TypeTag, UserPatchPB }

object Patches2:

  type Id[T] = T

  // https://medium.com/scala-3/scala-3-type-lambdas-polymorphic-function-types-and-dependent-function-types-2a6eabef896d
  type PermsMap = [K] =>> [V] =>> Map[K, V]

  opaque type UsrId <: Long = Long
  opaque type Permission <: String = String

  case class UserState(
      id: UsrId = -1L,
      siblings: immutable.Set[UsrId] = immutable.Set.empty,
      usrPermissions: immutable.Map[UsrId, Permission] = immutable.Map.empty)

  sealed trait Mutation[F[_], In]:
    def update(state: UserState)(newData: In): UserState

  object Mutation:

    given setUser: Mutation[Id, UsrId] = new:
      def update(state: UserState)(arg: UsrId): UserState =
        state.modify(_.id).setTo(arg)

    given addSibling: Mutation[Set, UsrId] = new:
      def update(state: UserState)(arg: UsrId): UserState =
        state.modify(_.siblings).using(_ + arg)

    given rmSibling: Mutation[Set, UsrId] = new:
      def update(state: UserState)(args: UsrId): UserState =
        state.modify(_.siblings).using(_ - args)

    given addPermission: Mutation[PermsMap[UsrId], (UsrId, Permission)] = new:
      def update(state: UserState)(args: (UsrId, Permission)): UserState =
        val userId = args._1
        val p = args._2
        state.modify(_.usrPermissions).using(_ + (userId -> p))

  end Mutation

  /** The [[Patches2.Patch]] structure takes the role of events from Event sourcing. It contains the information of
    * events but in a eDSL that is targeted to make a structural changes to the [[UserState]]. It describes how we allow
    * users to dive into different parts of the state and make changes.
    */
  enum Patch[State](val tag: TypeTag):
    self =>

    def ++(that: Patch[State]): Patch[State] = Patch.Both(self, that)

    case Both[State](a: Patch[State], b: Patch[State]) extends Patch[State](TypeTag.Unrecognized(-1))

    case SetUserId(userId: UsrId) extends Patch[State](TypeTag.SetUserIdTag)
    case AddSiblingId(sibId: UsrId) extends Patch[State](TypeTag.AddSiblingTag)
    case RemoveSiblingId(sibId: UsrId) extends Patch[State](TypeTag.RmSiblingTag)
    case AddUserPermission(userId: UsrId, permission: Permission) extends Patch[State](TypeTag.AddPermissionTag)

  end Patch

  object Patch:
    // constructors
    def setUserId(usr: UsrId): Patch[UserState] = SetUserId(usr)
    def addSibling(sib: UsrId): Patch[UserState] = AddSiblingId(sib)
    def rmSibling(sib: UsrId): Patch[UserState] = RemoveSiblingId(sib)
    def addPerm(usr: UsrId, p: Permission): Patch[UserState] = AddUserPermission(usr, p)

    def readLong(bytes: Array[Byte]): Long =
      (bytes(0) & 0xffL) << 56 |
        (bytes(1) & 0xffL) << 48 |
        (bytes(2) & 0xffL) << 40 |
        (bytes(3) & 0xffL) << 32 |
        (bytes(4) & 0xffL) << 24 |
        (bytes(5) & 0xffL) << 16 |
        (bytes(6) & 0xffL) << 8 |
        (bytes(7) & 0xffL)

    def writeLong(i: Long): Array[Byte] =
      val array = Array.ofDim[Byte](8)
      array(0) = (i >>> 56).toByte
      array(1) = (i >>> 48).toByte
      array(2) = (i >>> 40).toByte
      array(3) = (i >>> 32).toByte
      array(4) = (i >>> 24).toByte
      array(5) = (i >>> 16).toByte
      array(6) = (i >>> 8).toByte
      array(7) = i.toByte
      array

    def serialize[T <: Patch[?]](p: T): UserPatchPB =
      p match
        case SetUserId(userId) =>
          UserPatchPB(p.tag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(writeLong(userId)))
        case AddSiblingId(sId) =>
          UserPatchPB(p.tag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(writeLong(sId)))
        case RemoveSiblingId(sId) =>
          UserPatchPB(p.tag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(writeLong(sId)))
        case AddUserPermission(userId, permission) =>
          val pmBts = permission.getBytes(StandardCharsets.UTF_8)
          val bb = ByteBuffer.allocate(8 + pmBts.size)
          bb.putLong(userId)
          bb.put(pmBts)
          UserPatchPB(p.tag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(bb.array()))
        case Both(_, _) => throw new Exception("Cannot serialize Both !")

    def deserialize(pb: UserPatchPB): Patch[?] =
      pb.typeTag match
        case TypeTag.SetUserIdTag  => SetUserId(readLong(pb.payload.toByteArray))
        case TypeTag.AddSiblingTag => AddSiblingId(readLong(pb.payload.toByteArray))
        case TypeTag.RmSiblingTag  => RemoveSiblingId(readLong(pb.payload.toByteArray))
        case TypeTag.AddPermissionTag =>
          val bb = pb.payload.asReadOnlyByteBuffer()
          val usr = bb.getLong
          val bytes = new Array[Byte](bb.remaining())
          bb.get(bytes)
          AddUserPermission(usr, new String(bytes, StandardCharsets.UTF_8))
        case TypeTag.Unrecognized(_) => throw new Exception("Unrecognized TypeTag!")

    val maxStackSize = 1 << 9 // 20000

    def evalOptimizedRev(
        state: UserState,
        patch: Patch[UserState],
        acc: scala.collection.mutable.ArrayBuffer[Patch[UserState]] =
          new scala.collection.mutable.ArrayBuffer[Patch[UserState]],
      ): UserState =
      def evalState(state: UserState, acc: scala.collection.mutable.ArrayBuffer[Patch[UserState]]): UserState =
        var cur = state
        acc.foreach(m => cur = eval(cur, m))
        cur

      if (acc.size <= maxStackSize)
        patch match
          case both: Patch.Both[UserState] => evalOptimizedRev(state, both.a, acc.:+(both.b))
          case last                        => evalState(state, acc.:+(last))
      else
        val localState = evalState(state, acc)
        evalOptimizedRev(localState, patch)

    // apples in direct order inside batches
    def evalOptimized(
        state: UserState,
        patch: Patch[UserState],
        acc: List[Patch[UserState]] = Nil,
      ): UserState =
      def evalState(state: UserState, acc: List[Patch[UserState]]): UserState =
        var cur = state
        acc.foreach(m => cur = eval(cur, m))
        cur

      patch match
        case both: Patch.Both[UserState] =>
          if (acc.size <= maxStackSize) evalOptimized(state, both.b, both.a :: acc)
          else
            val localState = evalState(state, acc)
            evalOptimized(localState, patch, Nil)
        case one =>
          acc.headOption match
            case Some(value) =>
              value match
                case both: Patch.Both[UserState] =>
                  evalOptimized(state, both, one :: acc.tail)
                case _ =>
                  evalState(state, acc.head :: one :: acc.tail)
            case None => evalState(state, one :: acc)

    private def eval(state: UserState, m: Patch[UserState]): UserState =
      m match
        case both: Both[UserState] =>
          // reverse order
          // eval(eval(state, single), both)
          // direct order
          eval(eval(state, both.a), both.b)
        // stackoverflow
        case Patch.SetUserId(userId) => Mutation.setUser.update(state)(userId)
        // println(s"SetUserId(${userId})")
        case Patch.AddSiblingId(sibId)                   => Mutation.addSibling.update(state)(sibId)
        case Patch.RemoveSiblingId(sibId)                => Mutation.rmSibling.update(state)(sibId)
        case Patch.AddUserPermission(userId, permission) => Mutation.addPermission.update(state)((userId, permission))

    def evalRec(
        state: UserState,
        patch: Patch[UserState],
      ): scala.util.control.TailCalls.TailRec[UserState] =
      patch match
        case Patch.Both(cur, next) =>
          scala.util.control.TailCalls.tailcall(evalRec(state, cur)).flatMap(s => evalRec(s, next))
        case Patch.SetUserId(userId) =>
          scala.util.control.TailCalls.done(Mutation.setUser.update(state)(userId))
        case Patch.AddSiblingId(sibId) =>
          scala.util.control.TailCalls.done(Mutation.rmSibling.update(state)(sibId))
        case Patch.RemoveSiblingId(sibId) =>
          scala.util.control.TailCalls.done(Mutation.rmSibling.update(state)(sibId))
        case Patch.AddUserPermission(userId, permission) =>
          scala.util.control.TailCalls.done(Mutation.addPermission.update(state)((userId, permission)))

  end Patch

  def main(args: Array[String]) =
    import Patch.*

    val muts: Patch[UserState] =
      addPerm(2, "*") ++ addPerm(3, "**") ++ setUserId(1) ++ setUserId(2) ++ addSibling(2) ++ addSibling(
        3
      ) ++ rmSibling(1)

    println("serialize and deserialize: " + deserialize(serialize(addPerm(Long.MaxValue, "*"))))

    val s = evalRec(UserState(), muts).result
    val s1 = evalOptimizedRev(UserState(), muts)
    val s2 = evalOptimized(UserState(), muts)
    println(s)

  end main

end Patches2
