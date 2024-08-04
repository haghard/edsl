// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package edsl3

sealed trait Num[T] {
  def ++(a: T, b: T): T
  def --(a: T, b: T): T
}

object Num {
  given Integer: Num[Int] with {
    val code: Byte = 0x00
    def ++(a: Int, b: Int): Int = a + b
    def --(a: Int, b: Int): Int = a - b
    override val toString = "i"
  }

  given Dbl: Num[Double] with {
    val code: Byte = 0x01
    def ++(a: Double, b: Double) = a + b
    def --(a: Double, b: Double) = a - b

    override val toString = "d"
  }

}

// end Num

enum Exp[T] {
  case IntExp(v: Int) extends Exp[Int]
  case DblExt(v: Double) extends Exp[Double]
  case Sum[T](lhs: Exp[T], rhs: Exp[T])(using val ev: Num[T]) extends Exp[T]
  case Zip[A, B](lhs: Exp[A], rhs: Exp[B]) extends Exp[(A, B)]
}

object Exp {
  import Exp.*

  def eval[T](exp: Exp[T]): T =
    exp match {
      case IntExp(v) =>
        v
      case DblExt(v) =>
        v
      case s @ Sum(left, right) =>
        val l = eval(left)
        val r = eval(right)
        s.ev ++ (l, r)
      case zip: Zip[a, b] =>
        (eval(zip.lhs), eval(zip.rhs))
    }
}

// end Exp

@main def app(): Unit = Program()

object Program {
  import Exp.*

  def apply(): Unit = {
    val exp =
      Zip(
        Sum(IntExp(1), IntExp(2)),
        Sum(IntExp(2), IntExp(4)),
      )
    println(eval(exp))
  }
}

// end Program
