// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package patches

import com.softwaremill.quicklens.*
import schema.v1.{ TypeTag, UserPatchPB }

import scala.collection.*
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.compiletime.ops.int.*

object Patches {

  import compiletime.asMatchable
  // def f[X](x: X) = x.asMatchable match {}

  opaque type UsrId <: Int = Int
  opaque type Permission <: String = String

  enum UserPatch[T](val data: T, val protoTag: TypeTag) {
    self =>

    case SetUserId(userId: UsrId) extends UserPatch(userId, TypeTag.SetUserIdTag)
    case AddSibling(sibId: UsrId) extends UserPatch(sibId, TypeTag.AddPermissionTag)
    case RmSibling(sibId: UsrId) extends UserPatch(sibId, TypeTag.RmSiblingTag)
    case AddPermission(userId: UsrId, permission: Permission)
        extends UserPatch((userId, permission), TypeTag.AddPermissionTag)
  }

  // end UserPatch

  object UserPatch {

    def setUserId[T <: UsrId & Singleton, p >: T >= 1 <: true](userId: T): SetUserId = SetUserId(userId)
    def addSibling[T <: UsrId & Singleton, p >: T >= 1 <: true](sibId: T): AddSibling = AddSibling(sibId)
    def rmSibling[T <: UsrId & Singleton, p >: T >= 1 <: true](userId: T): RmSibling = RmSibling(userId)
    def addPermission[T <: UsrId & Singleton, p >: T >= 1 <: true](userId: T, p: Permission): AddPermission =
      AddPermission(userId, p)

    def toProto[T](p: UserPatch[T]): UserPatchPB =
      p match {
        case SetUserId(userId) =>
          UserPatchPB(p.protoTag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(UserPatch.writeInt(userId)))
        case AddSibling(sibId) =>
          UserPatchPB(p.protoTag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(UserPatch.writeInt(sibId)))
        case RmSibling(sibId) =>
          UserPatchPB(p.protoTag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(UserPatch.writeInt(sibId)))
        case AddPermission(userId, permission) =>
          val pmBts = permission.getBytes(StandardCharsets.UTF_8)
          val bb = ByteBuffer.allocate(4 + pmBts.size)
          bb.putInt(userId)
          bb.put(pmBts)
          UserPatchPB(p.protoTag, com.google.protobuf.UnsafeByteOperations.unsafeWrap(bb.array()))
      }

    def fromProto(p: UserPatchPB): UserPatch[?] =
      p.typeTag match {
        case TypeTag.SetUserIdTag  => SetUserId(readInt(p.payload.toByteArray))
        case TypeTag.AddSiblingTag => AddSibling(readInt(p.payload.toByteArray))
        case TypeTag.RmSiblingTag  => RmSibling(readInt(p.payload.toByteArray))
        case TypeTag.AddPermissionTag =>
          val bb = p.payload.asReadOnlyByteBuffer()
          val usr = bb.getInt
          val bytes = new Array[Byte](bb.remaining())
          bb.get(bytes)
          AddPermission(usr, new String(bytes, StandardCharsets.UTF_8))
        case TypeTag.Unrecognized(_) =>
          throw new Exception("Unrecognized TypeTag!")
      }

    def writeInt(i: Int): Array[Byte] = {
      val array = Array.ofDim[Byte](4)
      array(0) = (i >>> 24).toByte
      array(1) = (i >>> 16).toByte
      array(2) = (i >>> 8).toByte
      array(3) = i.toByte
      array
    }

    def readInt(bytes: Array[Byte]): Int =
      (bytes(0) << 24) |
        (bytes(1) & 0xff) << 16 |
        (bytes(2) & 0xff) << 8 |
        (bytes(3) & 0xff)

    /** polymorphic context function
      */
    def applyPatch(state: UserState): [Patch <: UserPatch[?]] => Patch => UserState =
      [Patch <: UserPatch[?]] =>
        (_: Patch) match {
          case UserPatch.SetUserId(userId) =>
            state.modify(_.id).setTo(userId)
          case UserPatch.AddSibling(userId) =>
            state.modify(_.siblings).using(_ + userId)
          case UserPatch.RmSibling(userId) =>
            state.modify(_.siblings).using(_ - userId)
          case UserPatch.AddPermission(userId, p) =>
            state.modify(_.usrPermissions).using(_ + (userId -> p))
      }
      // println(s"${classOf[UserPatch.AddPermission].getSimpleName}($userId,$p)")
  }

  // end UserPatch

  final case class UserState(
      id: UsrId = -1,
      siblings: immutable.Set[UsrId] = immutable.Set.empty,
      usrPermissions: immutable.Map[UsrId, Permission] = immutable.Map.empty) {
    override def toString(): String =
      s"User($id, sbs=[${siblings.mkString(",")}], perms=[${usrPermissions.mkString(",")}])"
  }

  def main(args: Array[String]): Unit = {
    import UserPatch.*

    val r = List(addPermission(1, "***"), setUserId(1), addSibling(2), addSibling(3))
      .foldLeft(UserState()) { (state, patch) =>
        println(fromProto(toProto(patch)))
        applyPatch(state)(patch)
      }

    println(r)
  }
}

// end Patches
