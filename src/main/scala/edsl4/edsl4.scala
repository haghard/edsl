// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package edsl4

import zio.constraintless.*
import TypeList.*
import scala.language.adhocExtensions

trait Num[T] {
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

  given Integer2: Num[(Int, Int)] with {
    val code: Byte = 0x01
    def sum(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1 + b._1, a._2 + b._2)
    def minus(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1 - b._1, a._2 - b._2)
    override val toString = "(i,i)"
  }

  given DblInt: Num[(Double, Int)] with {
    val code: Byte = 0x02
    def sum(a: (Double, Int), b: (Double, Int)): (Double, Int) = (a._1 + b._1, a._2 + b._2)
    def minus(a: (Double, Int), b: (Double, Int)): (Double, Int) = (a._1 - b._1, a._2 - b._2)
    override val toString = "(d,i)"
  }

  given IntDbl: Num[(Int, Double)] with {
    val code: Byte = 0x03
    def sum(a: (Int, Double), b: (Int, Double)): (Int, Double) = (a._1 + b._1, a._2 + b._2)
    def minus(a: (Int, Double), b: (Int, Double)): (Int, Double) = (a._1 - b._1, a._2 - b._2)
    override val toString = "(i,d)"
  }

  given DblIntInt: Num[((Double, Int), Int)] with {
    val code: Byte = 0x04
    def sum(a: ((Double, Int), Int), b: ((Double, Int), Int)): ((Double, Int), Int) =
      ((a._1._1 + b._1._1, a._1._2 + b._1._2), a._2 + b._2)
    def minus(a: ((Double, Int), Int), b: ((Double, Int), Int)): ((Double, Int), Int) =
      ((a._1._1 - b._1._1, a._1._2 - b._1._2), a._2 - b._2)
    override val toString = "((d,i),i)"
  }

  given Dbl: Num[Double] with {
    val code: Byte = 0x05
    def sum(a: Double, b: Double): Double = a + b
    def minus(a: Double, b: Double): Double = a - b
    override val toString = "d"
  }

  given Dbl2: Num[(Double, Double)] with {
    val code: Byte = 0x06
    def sum(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1 + b._1, a._2 + b._2)
    def minus(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1 - b._1, a._2 - b._2)
    override val toString = "d/d"
  }

  given Dbl3: Num[((Double, Double), Int)] with {
    val code: Byte = 0x07
    def sum(a: ((Double, Double), Int), b: ((Double, Double), Int)): ((Double, Double), Int) =
      ((a._1._1 + b._1._1, a._1._2 + b._1._2), a._2 + b._2)

    def minus(a: ((Double, Double), Int), b: ((Double, Double), Int)): ((Double, Double), Int) =
      ((a._1._1 - b._1._1, a._1._2 - b._1._2), a._2 - b._2)

    override val toString = "((d,d),i)"
  }

}

// end Num

enum Expr[TList <: TypeList, A] {
  self =>

  def +(expr: Expr[TList, A])(using ev: A `IsElementOf` TList): Expr[TList, A] = Sum(self, expr, ev)

  def -(expr: Expr[TList, A])(using ev: A `IsElementOf` TList): Expr[TList, A] = Minus(self, expr, ev)

  case Value[TList <: TypeList, A](a: A, ev: A `IsElementOf` TList) extends Expr[TList, A]

  case Prod[TList <: TypeList, A, B](
      lhs: Expr[TList, A],
      rhs: Expr[TList, B],
      ev: B `IsElementOf` TList) extends Expr[TList, (A, B)]

  case Sum[TList <: TypeList, A](
      lhs: Expr[TList, A],
      rhs: Expr[TList, A],
      ev: A `IsElementOf` TList) extends Expr[TList, A]

  case Minus[TList <: TypeList, A](
      lhs: Expr[TList, A],
      rhs: Expr[TList, A],
      ev: A `IsElementOf` TList) extends Expr[TList, A]

  def <>[B](expr: Expr[TList, B])(using ev: B `IsElementOf` TList): Expr[TList, (A, B)] =
    Prod(self, expr, ev)

  infix def zip[B](expr: Expr[TList, B])(using ev: B `IsElementOf` TList): Expr[TList, (A, B)] =
    <>(expr)
}

object Expr {

  def lit[A, TList <: TypeList](a: A)(using ev: A `IsElementOf` TList): Expr[TList, A] =
    Expr.Value(a, ev)

  def eval[TList <: TypeList, A](
      expr: Expr[TList, A]
    )(implicit ins: Instances[Num, TList]
    ): A =
    expr match {
      case Expr.Value(v, _) =>
        v
      case sum @ Expr.Sum(lhs, rhs, ev) =>
        def add[T](a: T, b: T): Num[T] => T = _.sum(a, b)
        ins.withInstance(add(eval(sum.lhs), eval(sum.rhs)))(ev)

      case Expr.Prod(lhs, rhs, _) =>
        (eval(lhs), eval(rhs))

      case m @ Expr.Minus(lhs, rhs, ev) =>
        def min[T](a: T, b: T): Num[T] => T = _.minus(a, b)
        ins.withInstance(min(eval(lhs), eval(rhs)))(ev)
    }
}

// end Expr

extension [A: Num, TList <: TypeList](v: A) {
  inline def lit(using n: Num[A], ev: A `IsElementOf` TList): Expr[TList, A] = Expr.Value(v, ev)
}

/** By deferring the resolution of type class instances until interpretation, ZIO Constraintless offers unparalleled
  * flexibility and modularity, empowering developers to customize DSLs while upholding type safety.
  *
  * runMain edsl4.app
  */
@main def app(): Unit = Program()

object Program {
  import Expr.*

  // All Types (combinations) the DSL supports
  type DslTypes = Double :: Int :: (Double, Double) :: (Double, Int) :: ((Double, Int), Int) ::
    ((Double, Double), Int) :: TypeList.End

  def apply(): Unit = {
    import Instances.*

    type RecordA = Expr[DslTypes, (Double, Int)]
    val a: RecordA = 1.47.lit <> 1.lit
    val b: RecordA = 1.3.lit <> 4.lit
    val c: RecordA = 1.3.lit zip 4.lit

    // summon[IsElementOf[(Double, Int), TypeList]]
    println(eval(a + b - c))

    val s: Expr[DslTypes, Int] = 3.lit
    val z: Expr[DslTypes, Double] = 3.1.lit
    val x: Expr[DslTypes, Double] = 3.2.lit

    type RecordB = Expr[DslTypes, ((Double, Double), Int)]
    val r0: RecordB = x <> z <> s
    var r1: RecordB = x zip z zip s
    val r2 = eval(r0 - r1)
    val r3 = eval(r0 + r1)
    println(r2)
    println(r3)

    val s1: Expr[DslTypes, Double] = 3.1.lit
    val z1: Expr[DslTypes, Int] = 3.lit
    val x1: Expr[DslTypes, Int] = 2.lit

    type RecordC = Expr[DslTypes, ((Double, Int), Int)]
    val res0: RecordC = s1 <> z1 <> x1
    var res1: RecordC = s1 zip z1 zip x1
    val res2 = eval(r0 - r1)
    val res3 = eval(r0 + r1)
    println(r2)
    println(r3)
  }
}

// end Program

/*
https://youtu.be/jtxO7LJXW0Q?list=LL
https://github.com/zio/zio-constraintless/blob/master/examples/shared/src/main/scala/zio/constraintless/examples/Expr.scala
 */
