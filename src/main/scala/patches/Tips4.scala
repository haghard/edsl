package patches

import scala.compiletime.summonInline
import scala.annotation.implicitNotFound

object Tips4 {

  enum MiniExpr[A] {
    case Bool(v: Boolean) extends MiniExpr[Boolean]

    case Int32(v: Int) extends MiniExpr[Int]

    case Int64(v: Long) extends MiniExpr[Long]

    case Sum[A](a: MiniExpr[A], b: MiniExpr[A]) extends MiniExpr[A]
  }

  @implicitNotFound("Missing One")
  trait Missing1

  @implicitNotFound("Missing Two")
  trait Missing2
  // given Missing2 = ???

  trait NotMissing
  given NotMissing = ???

  transparent inline def summonInlineCheck[T <: Int](inline t: T): Any =
    inline t match {
      case 1 => summonInline[Missing1]
      case 2 => summonInline[Missing2]
      case _ => summonInline[NotMissing]
    }

  // val missing1 = summonInlineCheck(1) // error: Missing One
  // val missing2 = summonInlineCheck(2) // error: Missing Two
  val notMissing: NotMissing = summonInlineCheck(3)
}
