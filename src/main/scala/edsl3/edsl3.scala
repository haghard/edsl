// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package edsl3

sealed trait Num[T] {
  def sum(a: T, b: T): T
  def minus(a: T, b: T): T
}

object Num {
  given Integer: Num[Int] with {
    val code: Byte = 0x00
    def sum(a: Int, b: Int): Int = a + b
    def minus(a: Int, b: Int): Int = a - b
    override val toString = "i"
  }

  given Dbl: Num[Double] with {
    val code: Byte = 0x01
    def sum(a: Double, b: Double) = a + b
    def minus(a: Double, b: Double) = a - b
    override val toString = "d"
  }

}

// end Num

enum Exp[T] {
  case Int32(v: Int) extends Exp[Int]
  case Int64(v: Long) extends Exp[Long]
  case Dbl(v: Double) extends Exp[Double]
  case Sum[T](lhs: Exp[T], rhs: Exp[T])(using val ev: Num[T]) extends Exp[T]
  case Minus[T](lhs: Exp[T], rhs: Exp[T])(using val ev: Num[T]) extends Exp[T]
  case Zip[A, B](lhs: Exp[A], rhs: Exp[B]) extends Exp[(A, B)]
}

object Exp {
  import Exp.*

  def eval[T](exp: Exp[T]): T =
    exp match {
      case Int32(v) =>
        v
      case Int64(v) =>
        v
      case Dbl(v) =>
        v
      case s @ Minus(left, right) =>
        s.ev.minus(eval(left), eval(right))
      case s @ Sum(left, right) =>
        s.ev.sum(eval(left), eval(right))
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
        Zip(
          Sum(Int32(1), Int32(2)),
          Sum(Int32(2), Int32(4)),
        ),
        Minus(Dbl(21.1), Dbl(4.25)),
      )

    println(eval(exp))
  }
}

// end Program
