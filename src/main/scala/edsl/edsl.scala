// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package edsl

import scala.util.control.NonFatal

/** Constraints a number of ways to build `NumTag` (only 3 ways)
  */
trait NumTag[T] {
  def code: Byte
}

object NumTag {

  given Integer: NumTag[Int] with {
    val code: Byte = 0x00
    override val toString = "i"
  }

  given Dbl: NumTag[Double] with {
    val code: Byte = 0x01
    override val toString = "d"
  }

  given Lng: NumTag[Long] with {
    val code: Byte = 0x02
    override val toString = "l"
  }
}

// end NumTag

trait Coercion[In, Out] {
  def from: NumTag[In]
  def to: NumTag[Out]
}

object Coercion {

  def convert[A, B](from: NumTag[A], to: NumTag[B]): A => B =
    (from, to) match {
      case (NumTag.Integer, NumTag.Dbl) => (in: Int) => in.toDouble
      case (NumTag.Dbl, NumTag.Integer) => (in: Double) => in.toInt
      case (NumTag.Integer, NumTag.Lng) => (in: Int) => in.toLong
    }

  given IntToDouble: Coercion[Int, Double] with {
    def from = NumTag.Integer
    def to = NumTag.Dbl
  }

  given DoubleToInt: Coercion[Double, Int] with {
    def from = NumTag.Dbl
    def to = NumTag.Integer
  }

  given IntToLong: Coercion[Int, Long] with {
    def from = NumTag.Integer
    def to = NumTag.Lng
  }
}

/** DslElement is an example of GADT.
  *
  * A GADT is a very specific example of a parametrically polymorphic ADT where you are allowed to specialize a type
  * parameter A in terms of a sum type. Specialization of the type A of DslElement inside the children together with
  * being able to reconstruct this information in pattern matching is known as GADTs.
  */
enum DslElement[A] {
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

  def !==(that: DslElement[A])(using N: NumTag[A]): DslElement[Boolean] = NotEq(self, that, N)

  def as[B](using c: Coercion[A, B]): DslElement[B] = Coercible[A, B](self, c)

  def d(using c: Coercion[A, Double]): DslElement[Double] = Coercible(self, c)

  def l(using c: Coercion[A, Long]): DslElement[Long] = Coercible(self, c)

  case Plus(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A]) extends DslElement[A]
  case Minus(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A]) extends DslElement[A]
  case Multiply(
      a: DslElement[A],
      b: DslElement[A],
      N: NumTag[A]) extends DslElement[A]

  case Negate(v: DslElement[A], N: NumTag[A]) extends DslElement[A]

  case Val(v: A, tag: NumTag[A]) extends DslElement[A]

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

  case NotEq(
      left: DslElement[A],
      right: DslElement[A],
      N: NumTag[A]) extends DslElement[Boolean]

  case IfThenElse(
      cond: DslElement[Boolean],
      ifTrue: DslElement[A],
      ifFalse: DslElement[A]) extends DslElement[A]
}

object DslElement {

  def If[A](
      when: DslElement[Boolean],
      ifTrue: DslElement[A],
      ifFalse: DslElement[A],
    ): DslElement[A] = IfThenElse(when, ifTrue, ifFalse)

  def serialize[T](v: DslElement[T]): String =
    v match {
      case DslElement.Plus(a, b, _) =>
        s"+|${serialize(a)}|${serialize(b)}"

      case DslElement.Minus(a, b, _) =>
        s"-|${serialize(a)}|${serialize(b)}"

      case DslElement.Multiply(a, b, _) =>
        s"*|${serialize(a)}|${serialize(b)}"

      case op: DslElement.Negate[T] =>
        s"Neg|${serialize(op.v)}"

      case op: DslElement.Coercible[in, out] =>
        val from = op.c.from.code
        val to = op.c.to.code
        s"Coerce:$from->$to|${serialize(op.v)}"

      case DslElement.Val(v, tag) =>
        s"${tag.code}:$v"

      case DslElement.Eq(left, right, _) =>
        s"${serialize(left)}==${serialize(right)})"

      case DslElement.NotEq(left, right, _) =>
        s"${serialize(left)}!=${serialize(right)})"

      case DslElement.GreaterThan(left, right, _) =>
        s">|${serialize(left)}|${serialize(right)}"

      case DslElement.GreaterOrEq(left, right, _) =>
        s">=|${serialize(left)}|${serialize(right)}"

      case DslElement.LesserThan(left, right, _) =>
        s"<|${serialize(left)}|${serialize(right)}"

      case DslElement.LesserOrEq(left, right, _) =>
        s"<=|${serialize(left)}|${serialize(right)}"

      case DslElement.IfThenElse(cond, ifTrue, ifFalse) =>
        s"if(${serialize(cond)}) ${serialize(ifTrue)} else ${serialize(ifFalse)}"
    }

  def deserialize(expr: String): DslElement[?] = ???

  def eval[T](v: DslElement[T]): T =
    v match {
      case op: DslElement.Plus[T] =>
        op.N match {
          case NumTag.Integer => eval(op.a) + eval(op.b)
          case NumTag.Dbl     => eval(op.a) + eval(op.b)
          case NumTag.Lng     => eval(op.a) + eval(op.b)
        }

      case op: DslElement.Minus[T] =>
        op.N match {
          case NumTag.Integer => eval(op.a) - eval(op.b)
          case NumTag.Dbl     => eval(op.a) - eval(op.b)
          case NumTag.Lng     => eval(op.a) - eval(op.b)
        }

      case op: DslElement.Multiply[T] =>
        op.N match {
          case NumTag.Integer => eval(op.a) * eval(op.b)
          case NumTag.Dbl     => eval(op.a) * eval(op.b)
          case NumTag.Lng     => eval(op.a) * eval(op.b)
        }

      case op: DslElement.Negate[T] =>
        op.N match {
          case NumTag.Integer => -eval(op.v)
          case NumTag.Dbl     => -eval(op.v)
          case NumTag.Lng     => -eval(op.v)
        }

      case op: DslElement.Val[T] => op.v

      case op: DslElement.Coercible[in, out] =>
        Coercion.convert(op.c.from, op.c.to)(eval(op.v))

      case op: DslElement.Eq[in] =>
        eval(op.left) == eval(op.right)

      case op: DslElement.LesserThan[in] =>
        op.N match {
          case NumTag.Integer => eval(op.left) < eval(op.right)
          case NumTag.Dbl     => eval(op.left) < eval(op.right)
          case NumTag.Lng     => eval(op.left) < eval(op.right)
        }

      case op: DslElement.LesserOrEq[in] =>
        op.N match {
          case NumTag.Integer => eval(op.left) <= eval(op.right)
          case NumTag.Dbl     => eval(op.left) <= eval(op.right)
          case NumTag.Lng     => eval(op.left) <= eval(op.right)
        }

      case op: DslElement.GreaterThan[in] =>
        op.N match {
          case NumTag.Integer => eval(op.left) > eval(op.right)
          case NumTag.Dbl     => eval(op.left) > eval(op.right)
          case NumTag.Lng     => eval(op.left) > eval(op.right)
        }

      case op: DslElement.GreaterOrEq[in] =>
        op.N match {
          case NumTag.Integer => eval(op.left) >= eval(op.right)
          case NumTag.Dbl     => eval(op.left) >= eval(op.right)
          case NumTag.Lng     => eval(op.left) >= eval(op.right)
        }

      case op: DslElement.NotEq[in] =>
        eval(op.left) != eval(op.right)

      case DslElement.IfThenElse(cond, ifTrue, ifFalse) =>
        if (eval(cond)) eval(ifTrue) else eval(ifFalse)
    }
}

//This env expect (a:int,b:int,c:dbl,d:long)
object StaticEnvDsl {
  import scala.compiletime.*
  import scala.compiletime.ops.string.*
  import scala.compiletime.ops.string.{ CharAt, Length, Substring }
  import scala.Tuple.*
  import scala.compiletime.ops.*

  // https://blog.rockthejvm.com/practical-type-level-programming/
  // https://github.com/ncreep/scala3-flat-json-blog
  type ++[A, B] <: String = (A, B) match { case (a, b) => a + b }
  type MkLine[T <: Tuple] = Fold[Init[T], Last[T], [a, b] =>> a ++ "," ++ b]
  type Vars = ("a", "b", "c", "d")

  type Args[S <: String] <: Tuple =
    S match {
      case "" =>
        EmptyTuple
      case _ =>
        CharAt[S, 0] match {
          case 'a' | 'b' =>
            Int *: Args[Substring[S, 1, Length[S]]]
          case 'c' =>
            Double *: Args[Substring[S, 1, Length[S]]]
          case 'd' =>
            Long *: Args[Substring[S, 1, Length[S]]]
          case ',' =>
            Args[Substring[S, 1, Length[S]]]
          case _ =>
            Args[Substring[S, 1, Length[S]]]
        }
    }

  def init(varNames: String)(varValues: Args[varNames.type]): scala.collection.mutable.Map[String, Any] = {
    val values = varValues.toList

    val envVars = new scala.collection.mutable.HashMap[String, Any]()
    varNames.split(',').zipWithIndex.foreach {
      case (name, i) =>
        envVars.put(name, values(i))
    }

    println(s"""
         |★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★
         |[${constValue[StaticEnvDsl.MkLine[StaticEnvDsl.Vars]]}] ✅
         |----------------------------------------+
         |[${envVars.mkString(",")}] ✅
         |★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★
         |""".stripMargin)

    envVars
  }
}

object DslEnv {
  import scala.compiletime.*
  import scala.compiletime.ops.string.Matches
  import scala.compiletime.ops.*
  import compiletime.constValue
  import compiletime.ops.int.S

  type Out[T <: String] =
    T match {
      case _ =>
        Matches[T, "^[a-z]*(\\s)*\\:(\\s)*int"] match {
          case true =>
            Int
          case false =>
            Matches[T, "^[a-z]*(\\s)*\\:(\\s)*long"] match {
              case true =>
                Long
              case false =>
                Matches[T, "^[a-z]*(\\s)*\\:(\\s)*double"] match {
                  case true =>
                    Double
                  case false =>
                    Nothing
                }
            }
        }
    }

  inline def checkVar(typedVar: String) =
    inline if !constValue[Matches[typedVar.type, "^[a-z]*(\\s)*\\:(\\s)*[int|dbl|long]*"]]
    then error("Invalid definition" + codeOf(typedVar))

  //
  val vars: scala.collection.mutable.Map[String, Any] =
    scala.collection.mutable.Map[String, Any]()

  def setVar(typedVar: String)(varValues: Out[typedVar.type]): Unit /*Out[typedVar.type]*/ = {
    val name = typedVar.takeWhile(_ != ':')
    vars.put(name.trim, varValues)
  }

  def print() =
    println(s"""
         |★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★
         |[${vars.mkString(",")}] ✅
         |★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★ ★
         |""".stripMargin)
}

extension [A: NumTag](v: A) {
  inline def num(using tag: NumTag[A]): DslElement[A] =
    DslElement.Val(v, tag)
}

extension (v: String) {

  def env[A: NumTag](using tag: NumTag[A], vars: scala.collection.mutable.Map[String, Any]): DslElement[A] =
    tag match {
      case NumTag.Integer => DslElement.Val(vars(v).asInstanceOf[Int], tag)
      case NumTag.Dbl     => DslElement.Val(vars(v).asInstanceOf[Double], tag)
      case NumTag.Lng     => DslElement.Val(vars(v).asInstanceOf[Long], tag)
    }

  def int(using vars: scala.collection.mutable.Map[String, Any]): DslElement[Int] =
    DslElement.Val(vars(v).asInstanceOf[Int], NumTag.Integer)

  def lng(using vars: scala.collection.mutable.Map[String, Any]): DslElement[Long] =
    DslElement.Val(vars(v).asInstanceOf[Long], NumTag.Lng)

  def dbl(using vars: scala.collection.mutable.Map[String, Any]): DslElement[Double] =
    DslElement.Val(vars(v).asInstanceOf[Double], NumTag.Dbl)

  def parse[A: NumTag](using tag: NumTag[A]): DslElement[A] =
    tag match {
      case NumTag.Integer => DslElement.Val(v.toInt, tag)
      case NumTag.Dbl     => DslElement.Val(v.toDouble, tag)
      case NumTag.Lng     => DslElement.Val(v.toLong, tag)
    }
}

@main def app(): Unit =
  // Program()
  Program2()

object Program {
  import DslElement.*
  import scala.compiletime.*
  import scala.compiletime.constValueTuple

  given envVars: scala.collection.mutable.Map[String, Any] =
    StaticEnvDsl.init(constValue[StaticEnvDsl.MkLine[StaticEnvDsl.Vars]])(3, 45, Double.MaxValue, Long.MaxValue)
  // StaticEnv.init("a,b,d,c")(1, 45, 9L, 0.2)

  def apply(): Unit =
    try {
      val envConstValues = constValueTuple[StaticEnvDsl.Vars]

      val exp0 = 1.num + 5.num * 10.6.num.as[Int]
      val exp1 = 1.67.num * 10.num.as[Double] + 89.num.as[Double]
      val exp2 = "1.67".parse[Double] + 10.num.as[Double]

      val result0 = eval(exp0)
      println(result0)
      val result1 = eval(exp1)
      println(result1)
      val result2 = eval(exp2)
      println(result2)

      println(serialize(exp1))
      println(eval(10.6.num === 10.num))

      println(serialize(1.num === 10.6.num.as[Int]))

      val exp = If(11.num.===(11.num), 1.6.num, 2.1.num - 1.num)

      println(serialize(exp))
      println(eval(exp))

      val exp3 = If(2.1.num <= 1.56.num, If(2.num < 1.num, 1.num, 0.num), -(2.num + -1.num))
      println(serialize(exp3))
      println(eval(exp3))

      val exp4 = If("a".env[Int] >= 0.num, 1.num, 0.num)
      println(serialize(exp4))
      println(eval(exp4))

      val exp5 = If("a".int >= "b".int, 1.num, 0.num)
      println(serialize(exp5))
      println(eval(exp5))

      val (a, b, c, d) = envConstValues
      If(a.int !== b.int, 1.num, 0.num)

      val exp6 = If(a.int >= b.int, 1.num, 0.num)
      println(serialize(exp6))
      println(eval(exp6))

    }
    catch {
      case NonFatal(ex) => ex.printStackTrace
    }
}

object Program2 {
  import DslElement.*

  // DslEnv.checkVar("a§b: int")

  /*
  val aEnv = DslEnv.setVar("a: int")(3)
  val bEnv = DslEnv.setVar("b :long")(78L)
  val cEnv = DslEnv.setVar("c:double")(.8)
  val dEnv = DslEnv.setVar("d :  int")(88)
   */

  DslEnv.setVar("a: int")(-31)
  DslEnv.setVar("b :int")(31)
  DslEnv.setVar("c:double")(.8)
  DslEnv.setVar("d :  long")(Long.MaxValue)

  DslEnv.print()
  given envVars: scala.collection.mutable.Map[String, Any] = DslEnv.vars

  def apply(): Unit =
    try {
      val exp0 = If(-"a".env[Int] !== "b".env[Int], 1.num, 0.num)
      println(serialize(exp0) + " = " + eval(exp0))
    }
    catch {
      case NonFatal(ex) => ex.printStackTrace
    }
}

// Todo: serialization with protobuf
