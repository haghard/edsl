// Copyright (c) 2021-22 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package edsl

import scala.util.control.NonFatal

/** Constraints a number of ways to build `NumTag` (only 3 ways)
  */
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

  def convert[A, B](from: NumTag[A], to: NumTag[B]): A => B =
    (from, to) match
      case (NumTag.Integer, NumTag.Dbl) => (in: Int) => in.toDouble
      case (NumTag.Dbl, NumTag.Integer) => (in: Double) => in.toInt
      case (NumTag.Integer, NumTag.Lng) => (in: Int) => in.toLong

  given IntToDouble: Coercion[Int, Double] with
    def from = NumTag.Integer
    def to = NumTag.Dbl

  given DoubleToInt: Coercion[Double, Int] with
    def from = NumTag.Dbl
    def to = NumTag.Integer

  given IntToLong: Coercion[Int, Long] with
    def from = NumTag.Integer
    def to = NumTag.Lng

/** DslElement is an example of GADT.
  *
  * A GADT is a very specific example of a parametrically polymorphic ADT where you are allowed to specialize a type
  * parameter A in terms of the sum type. Specialization of the type A of DslElement inside the children together with
  * being able to reconstruct this information in pattern matching is known as GADTs.
  */
enum DslElement[A]:
  self =>

  def +(that: DslElement[A])(using N: NumTag[A]): DslElement[A] =
    Plus(self, that, N)

  def -(that: DslElement[A])(using N: NumTag[A]): DslElement[A] =
    Minus(self, that, N)

  def *(that: DslElement[A])(using N: NumTag[A]): DslElement[A] =
    Multiply(self, that, N)

  def unary_-(using N: NumTag[A]): DslElement[A] = Negate(self, N)

  def ===(that: DslElement[A])(using N: NumTag[A]): DslElement[Boolean] = Eq(self, that, N)

  def >(that: DslElement[A])(using N: NumTag[A]): DslElement[Boolean] = GreaterThan(self, that, N)

  def >=(that: DslElement[A])(using N: NumTag[A]): DslElement[Boolean] = GreaterOrEq(self, that, N)

  def <(that: DslElement[A])(using N: NumTag[A]): DslElement[Boolean] = LesserThan(self, that, N)

  def <=(that: DslElement[A])(using N: NumTag[A]): DslElement[Boolean] = LesserOrEq(self, that, N)

  def as[B](using c: Coercion[A, B]): DslElement[B] = Coercible[A, B](self, c)

  def d(using c: Coercion[A, Double]): DslElement[Double] = Coercible(self, c)

  def l(using c: Coercion[A, Long]): DslElement[Long] = Coercible(self, c)

  case Plus(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A])
  case Minus(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A])
  case Multiply(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A])

  case Negate(v: DslElement[A], N: NumTag[A])
  case Val(v: A, tag: NumTag[A])
  case Coercible[A, B](v: DslElement[A], c: Coercion[A, B]) extends DslElement[B]

  case Eq(
      left: DslElement[A],
      right: DslElement[A],
      N: NumTag[A]) extends DslElement[Boolean]

  case GreaterThan(
      left: DslElement[A],
      right: DslElement[A],
      N: NumTag[A]) extends DslElement[Boolean]

  case GreaterOrEq(
      left: DslElement[A],
      right: DslElement[A],
      N: NumTag[A]) extends DslElement[Boolean]

  case LesserThan(
      left: DslElement[A],
      right: DslElement[A],
      N: NumTag[A]) extends DslElement[Boolean]

  case LesserOrEq(
      left: DslElement[A],
      right: DslElement[A],
      N: NumTag[A]) extends DslElement[Boolean]

  case Cond(
      cond: DslElement[Boolean],
      ifTrue: DslElement[A],
      ifFalse: DslElement[A]) extends DslElement[A]

object DslElement:

  def If[A](
      cond: DslElement[Boolean],
      ifTrue: DslElement[A],
      ifFalse: DslElement[A],
    ): DslElement[A] = Cond(cond, ifTrue, ifFalse)

  def serialize[T](v: DslElement[T]): String =
    v match
      case DslElement.Plus(a, b, _) =>
        s"+|${serialize(a)}|${serialize(b)}"

      case DslElement.Minus(a, b, _) =>
        s"-|${serialize(a)}|${serialize(b)}"

      case DslElement.Multiply(a, b, _) =>
        s"*|${serialize(a)}|${serialize(b)}"

      case op: DslElement.Negate[T] =>
        s"neg|${serialize(op.v)}"

      case op: DslElement.Coercible[in, out] =>
        val from = op.c.from.code
        val to = op.c.to.code
        s"COERCE_TO:$from->$to|${serialize(op.v)}"

      case DslElement.Val(v, tag) =>
        s"${tag.code}:$v"

      case DslElement.Eq(left, right, _) =>
        s"${serialize(left)}==${serialize(right)})"

      case DslElement.GreaterThan(left, right, _) =>
        s">|${serialize(left)}|${serialize(right)}"

      case DslElement.GreaterOrEq(left, right, _) =>
        s">=|${serialize(left)}|${serialize(right)}"

      case DslElement.LesserThan(left, right, _) =>
        s"<|${serialize(left)}|${serialize(right)}"

      case DslElement.LesserOrEq(left, right, _) =>
        s"<=|${serialize(left)}|${serialize(right)}"

      case DslElement.Cond(cond, ifTrue, ifFalse) =>
        s"if(${serialize(cond)}) ${serialize(ifTrue)} else ${serialize(ifFalse)}"

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

      case op: DslElement.Eq[in] =>
        eval(op.left) == eval(op.right)

      case op: DslElement.LesserThan[in] =>
        op.N match
          case NumTag.Integer => eval(op.left) < eval(op.right)
          case NumTag.Dbl     => eval(op.left) < eval(op.right)
          case NumTag.Lng     => eval(op.left) < eval(op.right)

      case op: DslElement.LesserOrEq[in] =>
        op.N match
          case NumTag.Integer => eval(op.left) <= eval(op.right)
          case NumTag.Dbl     => eval(op.left) <= eval(op.right)
          case NumTag.Lng     => eval(op.left) <= eval(op.right)

      case op: DslElement.GreaterThan[in] =>
        op.N match
          case NumTag.Integer => eval(op.left) > eval(op.right)
          case NumTag.Dbl     => eval(op.left) > eval(op.right)
          case NumTag.Lng     => eval(op.left) > eval(op.right)

      case op: DslElement.GreaterOrEq[in] =>
        op.N match
          case NumTag.Integer => eval(op.left) >= eval(op.right)
          case NumTag.Dbl     => eval(op.left) >= eval(op.right)
          case NumTag.Lng     => eval(op.left) >= eval(op.right)

      case DslElement.Cond(cond, ifTrue, ifFalse) =>
        if (eval(cond)) eval(ifTrue) else eval(ifFalse)

extension [A: NumTag](v: A)
  inline def lit(using tag: NumTag[A]): DslElement[A] =
    DslElement.Val(v, tag)

extension (v: String)
  def parse[A: NumTag](using tag: NumTag[A]): DslElement[A] =
    tag match
      case NumTag.Integer => DslElement.Val(v.toInt, tag)
      case NumTag.Dbl     => DslElement.Val(v.toDouble, tag)
      case NumTag.Lng     => DslElement.Val(v.toLong, tag)

@main def app(): Unit = Program()

object Program:

  import NumTag.*
  import DslElement.*

  def apply(): Unit =
    try
      val exp0 = 1.lit + 5.lit * 10.6.lit.as[Int]
      val exp1 = 1.67.lit * 10.lit.as[Double] + 89.lit.as[Double]
      val exp2 = "1.67".parse[Double] + 10.lit.as[Double]

      val result0 = eval(exp0)
      println(result0)
      val result1 = eval(exp1)
      println(result1)
      val result2 = eval(exp2)
      println(result2)

      println(serialize(exp1))
      println(eval(10.6.lit === 10.lit))

      println(serialize(1.lit === 10.6.lit.as[Int]))

      val exp = If(11.lit.===(11.lit), 1.6.lit, 2.1.lit - 1.lit)

      println(serialize(exp))
      println(eval(exp))

      val exp3 = If(2.1.lit <= 1.56.lit, If(2.lit < 1.lit, 1.lit, 0.lit), -(2.lit + -1.lit))

      println(serialize(exp3))
      println(eval(exp3))

    catch { case NonFatal(ex) => ex.printStackTrace }
