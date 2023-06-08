// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package patches

import scala.collection.immutable
import scala.compiletime.ops.long.*
import com.softwaremill.quicklens.*

import compiletime.asMatchable
import scala.reflect.ClassTag
import schema.v1.{ TypeTag, UserPatchPB }
import Patches3.UsrId.toBts

object Patches3 {

  opaque type UsrId /*<: Long*/ = Long

  object UsrId {
    def apply(n: Long): UsrId = n
    extension (u: UsrId) {
      def toBts(): Array[Byte] = {
        // Guava Longs
        val array = Array.ofDim[Byte](8)
        array(0) = (u >>> 56).toByte
        array(1) = (u >>> 48).toByte
        array(2) = (u >>> 40).toByte
        array(3) = (u >>> 32).toByte
        array(4) = (u >>> 24).toByte
        array(5) = (u >>> 16).toByte
        array(6) = (u >>> 8).toByte
        array(7) = u.toByte
        array
      }
    }
  }

  // end UsrId

  opaque type Permission <: String = String

  trait Codec[T <: TypeTag] {
    def toProto(p: Patch[T]): UserPatchPB
    def fromProto(pb: UserPatchPB): Patch[T]
  }

  object Codec {
    def fromBts(bytes: Array[Byte]): UsrId =
      (bytes(0) & 0xffL) << 56 |
        (bytes(1) & 0xffL) << 48 |
        (bytes(2) & 0xffL) << 40 |
        (bytes(3) & 0xffL) << 32 |
        (bytes(4) & 0xffL) << 24 |
        (bytes(5) & 0xffL) << 16 |
        (bytes(6) & 0xffL) << 8 |
        (bytes(7) & 0xffL)

    given a: Codec[TypeTag.SetUserIdTag.type] with {
      def toProto(p: Patch[TypeTag.SetUserIdTag.type]): UserPatchPB =
        p.asMatchable match {
          case Patch.SetUserId(userId) =>
            UserPatchPB(TypeTag.SetUserIdTag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(userId.toBts()))
          case _ => throw new Exception("Boom!")
        }

      def fromProto(pb: UserPatchPB): Patch.SetUserId =
        Patch.SetUserId(fromBts(pb.payload.toByteArray))
    }

    given b: Codec[TypeTag.AddSiblingTag.type] with {
      def toProto(p: Patch[TypeTag.AddSiblingTag.type]): UserPatchPB = ???
      def fromProto(pb: UserPatchPB): Patch[TypeTag.AddSiblingTag.type] = ???
    }

    given c: Codec[TypeTag.RmSiblingTag.type] with {
      def toProto(p: Patch[TypeTag.RmSiblingTag.type]): UserPatchPB = ???
      def fromProto(pb: UserPatchPB): Patch[TypeTag.RmSiblingTag.type] = ???
    }

    given d: Codec[TypeTag.AddPermissionTag.type] with {
      def toProto(p: Patch[TypeTag.AddPermissionTag.type]): UserPatchPB = ???
      def fromProto(pb: UserPatchPB): Patch[TypeTag.AddPermissionTag.type] = ???
    }
  }

  // end Codec

  // trait Mut[T <: TypeTag & Singleton]:
  trait Mut[T <: TypeTag] {
    type In
    def update(state: State, userId: In): State
  }

  object Mut {
    given a: Mut[TypeTag.SetUserIdTag.type] with {
      type In = UsrId
      def update(state: State, userId: In): State =
        state.modify(_.id).setTo(userId)
    }

    given b: Mut[TypeTag.AddSiblingTag.type] with {
      type In = UsrId
      def update(state: State, sid: In): State =
        state.modify(_.siblings).using(_ + sid)
    }

    given c: Mut[TypeTag.RmSiblingTag.type] with {
      type In = UsrId
      def update(state: State, sid: In): State =
        state.modify(_.siblings).using(_ + sid)
    }

    given d: Mut[TypeTag.AddPermissionTag.type] with {
      type In = (UsrId, Permission)
      def update(state: State, args: In): State =
        state.modify(_.usrPermissions).using(_ + args)
    }
  }

  // end Mut

  /*
  trait Mut0[T <: TypeTag]:
    type In
    def argClassName: String
    def update(state: State)(arg: In): State
    def coerce(in: Any): In

  end Mut0

  object Mut0:

    given a: Mut0[TypeTag.SetUserIdTag.type] with
      type In = UsrId
      def argClassName: String = classOf[In].getSimpleName
      def update(state: State)(userId: In) = state.modify(_.id).setTo(userId)
      def coerce(in: Any): In =
        in.asMatchable match
          case v: In => v
          case other => throw new Exception(s"Unexpected ${other.getClass.getSimpleName} - Expected $argClassName")

    given d: Mut0[TypeTag.AddPermissionTag.type] with
      type In = (UsrId, Permission)
      def argClassName: String = classOf[In].getSimpleName
      def update(state: State)(args: In) = state.modify(_.usrPermissions).using(_ + args)
      inline def coerce(in: Any): In =
        in.asMatchable match
          case v: In => v
          case other => throw new Exception(s"Unexpected ${other.getClass.getSimpleName} - Expected $argClassName")

  end Mut0
   */

  enum Patch[T <: TypeTag] {
    self =>

    case SetUserId(userId: UsrId) extends Patch[TypeTag.SetUserIdTag.type]
    case AddSibling(sibId: UsrId) extends Patch[TypeTag.AddSiblingTag.type]
    case RmSibling(sibId: UsrId) extends Patch[TypeTag.RmSiblingTag.type]
    case AddPermission(userId: UsrId, ps: Permission) extends Patch[TypeTag.AddPermissionTag.type]
  }

  // end Patch

  object Patch {

    // constructors
    def setUserId(userId: UsrId) = SetUserId(userId)
    def addSibling(sId: UsrId) = AddSibling(sId)
    def rmSibling(sId: UsrId) = RmSibling(sId)
    def addPermission(userId: UsrId, ps: Permission) = AddPermission(userId, ps)

    def serialize[T <: TypeTag](p: Patch[T])(using C: Codec[T]): UserPatchPB = C.toProto(p)

    def deserialize(pb: UserPatchPB) =
      pb.typeTag match {
        case TypeTag.SetUserIdTag     => summon[Codec[TypeTag.SetUserIdTag.type]].fromProto(pb)
        case TypeTag.AddSiblingTag    => summon[Codec[TypeTag.AddSiblingTag.type]].fromProto(pb)
        case TypeTag.RmSiblingTag     => summon[Codec[TypeTag.RmSiblingTag.type]].fromProto(pb)
        case TypeTag.AddPermissionTag => summon[Codec[TypeTag.AddPermissionTag.type]].fromProto(pb)
        case TypeTag.Unrecognized(v)  => throw new Exception(s"Unrecognized($v) value !!!")
      }

    private def mut[T <: TypeTag](
        tag: T,
        state: State,
      )(using M: Mut[tag.type]
      ): (M.In) => State =
      (arg: M.In) => M.update(state, arg)

    // This interpreter is correct by construction.
    def eval[T <: TypeTag](state: State, p: Patch[T]): State =
      p match {
        case Patch.SetUserId(userId)       => mut(TypeTag.SetUserIdTag, state)(userId)
        case Patch.AddSibling(sid)         => mut(TypeTag.AddSiblingTag, state)(sid)
        case Patch.RmSibling(sid)          => mut(TypeTag.RmSiblingTag, state)(sid)
        case Patch.AddPermission(user, ps) => mut(TypeTag.AddPermissionTag, state)((user, ps))
      }
  }

  // This interpreter may throw in runtime
  /*def eval0[T <: TypeTag](state: State, p: Patch[T])(using M: Mut0[T]): State =
      p match
        case Patch.SetUserId(userId) =>
          M.coerce(12) // it accepts 12 as an int but throws in runtime.
          M.update(state)(M.coerce(userId))
        case Patch.AddSibling(sid)         => M.update(state)(M.coerce(sid))
        case Patch.RmSibling(sid)          => M.update(state)(M.coerce(sid))
        case Patch.AddPermission(user, ps) => M.update(state)(M.coerce((user, ps)))*/

  // end Patch

  final case class State(
      id: UsrId = -1L, // a primitive type
      siblings: Set[UsrId] = immutable.Set.empty, // Set
      usrPermissions: Map[UsrId, Permission] = immutable.Map.empty) {
    override def toString(): String =
      s"User($id, sbs=[${siblings.mkString(",")}], perms=[${usrPermissions.mkString(",")}])"
  }

  def main(args: Array[String]) = {
    import Patch.*

    println(deserialize(serialize(setUserId(999L))))

    val r = List(setUserId(1L), addSibling(2L), addSibling(3L), addPermission(1L, "***"))
      .foldLeft(State()) { (acc, c) =>
        Patch.eval(acc, c)
      }
    println(r)
  }
}

// end main

// end Patches3
