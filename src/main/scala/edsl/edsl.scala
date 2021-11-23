// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package edsl

import scala.util.control.NonFatal

trait NumTag[T]:
  def code: Byte

object NumTag:
  given Integer: NumTag[Int] = new NumTag[Int] {
    val code: Byte = 0x00
    override val toString = "i"
  }
  given Dbl: NumTag[Double] = new NumTag[Double] {
    val code: Byte = 0x01
    override val toString = "d"
  }
  given Lng: NumTag[Long] = new NumTag[Long] {
    val code: Byte = 0x02
    override val toString = "l"
  }
end NumTag

trait Coercion[In, Out]:
  def from: NumTag[In]
  def to: NumTag[Out]

object Coercion:

  def convert[A, B](from: NumTag[A], to: NumTag[B]): A ⇒ B =
    (from, to) match
      case (NumTag.Integer, NumTag.Dbl) => (in: Int) => in.toDouble
      case (NumTag.Integer, NumTag.Lng) => (in: Int) => in.toLong
      case (NumTag.Dbl, NumTag.Integer) => (in: Double) => in.toInt

  given IntToDouble: Coercion[Int, Double] with
    def from = NumTag.Integer
    def to = NumTag.Dbl

  given IntToLong: Coercion[Int, Long] with
    def from = NumTag.Integer
    def to = NumTag.Lng

  given DoubleToInt: Coercion[Double, Int] with
    def from = NumTag.Dbl
    def to = NumTag.Integer

enum DslElement[A]:
  self ⇒

  def +(that: DslElement[A])(using N: NumTag[A]): DslElement[A] =
    Plus(self, that, N)

  def -(that: DslElement[A])(using N: NumTag[A]): DslElement[A] =
    Minus(self, that, N)

  def *(that: DslElement[A])(using N: NumTag[A]): DslElement[A] =
    Multiply(self, that, N)

  def as[B](using c: Coercion[A, B]): DslElement[B] = Coercible[A, B](self, c)

  def d(using c: Coercion[A, Double]): DslElement[Double] = Coercible(self, c)

  def l(using c: Coercion[A, Long]): DslElement[Long] = Coercible(self, c)

  case Plus(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A],
    )
  case Minus(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A],
    )
  case Multiply(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A],
    )

  case Negate(v: DslElement[A], N: NumTag[A])
  case Val(v: A, tag: NumTag[A])
  case Coercible[A, B](v: DslElement[A], c: Coercion[A, B]) extends DslElement[B]

object DslElement:

  def serialize[T](v: DslElement[T]): String =
    v match
      case DslElement.Plus(a, b, _) =>
        s"Pl|${serialize(a)}|${serialize(b)}"

      case DslElement.Minus(a, b, _) =>
        s"Min|${serialize(a)}|${serialize(b)}"

      case DslElement.Multiply(a, b, _) =>
        s"Mul|${serialize(a)}|${serialize(b)}"

      case op: DslElement.Negate[T] =>
        s"Neg|${serialize(op.v)}"

      case op: DslElement.Coercible[in, out] =>
        val from = op.c.from.code
        val to = op.c.to.code
        s"COERCE_TO:$from->$to|${serialize(op.v)}"

      case DslElement.Val(v, tag) =>
        s"${tag.code}:$v"

  def deserialize(expr: String): DslElement[?] = ???

  def eval[T](v: DslElement[T]): T =
    v match
      case op: DslElement.Plus[T] =>
        op.N match
          case NumTag.Integer => eval(op.a) + eval(op.b)
          case NumTag.Dbl     => eval(op.a) + eval(op.b)
          case NumTag.Lng     => eval(op.a) + eval(op.b)

      case op: DslElement.Minus[T] =>
        op.N match
          case NumTag.Integer => eval(op.a) - eval(op.b)
          case NumTag.Dbl     => eval(op.a) - eval(op.b)
          case NumTag.Lng     => eval(op.a) - eval(op.b)

      case op: DslElement.Multiply[T] =>
        op.N match
          case NumTag.Integer => eval(op.a) * eval(op.b)
          case NumTag.Dbl     => eval(op.a) * eval(op.b)
          case NumTag.Lng     => eval(op.a) * eval(op.b)

      case op: DslElement.Negate[T] =>
        op.N match
          case NumTag.Integer => -eval(op.v)
          case NumTag.Dbl     => -eval(op.v)
          case NumTag.Lng     => -eval(op.v)

      case op: DslElement.Val[T] => op.v

      case op: DslElement.Coercible[in, out] =>
        Coercion.convert(op.c.from, op.c.to)(eval(op.v))

extension [A: NumTag](x: A)
  def lit(using tag: NumTag[A]): DslElement[A] =
    DslElement.Val(x, tag)

extension (x: String)
  def parse[A: NumTag](using tag: NumTag[A]): DslElement[A] =
    tag match
      case NumTag.Integer ⇒ DslElement.Val(x.toInt, tag)
      case NumTag.Dbl ⇒ DslElement.Val(x.toDouble, tag)
      case NumTag.Lng ⇒ DslElement.Val(x.toLong, tag)

@main def app(): Unit = Program()

object Program:

  import NumTag.*

  def apply(): Unit =
    try

      val exp0 = 1.lit + 5.lit * 10.6.lit.as[Int]
      val exp1 = 1.67.lit * 10.lit.as[Double] + 89.lit.as[Double]
      val exp2 = "1.67".parse[Double] + 10.lit.as[Double]

      val result0 = DslElement.eval(exp0)
      println(result0)
      val result1 = DslElement.eval(exp1)
      println(result1)
      val result2 = DslElement.eval(exp2)
      println(result2)

      println(DslElement.serialize(exp1))

    catch { case NonFatal(ex) ⇒ ex.printStackTrace }