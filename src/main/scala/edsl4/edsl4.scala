package edsl4

import zio.constraintless.*
import TypeList.*

trait Num[T]:
  def sum(a: T, b: T): T
  def minus(a: T, b: T): T

object Num:
  given Integer: Num[Int] with
    val code: Byte = 0x00
    def sum(a: Int, b: Int): Int = a + b
    def minus(a: Int, b: Int): Int = a - b
    override val toString = "i"

  given Dbl: Num[Double] with
    val code: Byte = 0x01
    def sum(a: Double, b: Double): Double = a + b
    def minus(a: Double, b: Double): Double = a - b
    override val toString = "d"

  given Dbl2: Num[(Double, Double)] with
    val code: Byte = 0x02
    def sum(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1 + b._1, a._2 + b._2)
    def minus(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1 - b._1, a._2 - b._2)
    override val toString = "d2"

  given Dbl3: Num[(Int | Double)] with
    val code: Byte = 0x02
    def sum(a: (Int | Double), b: (Int | Double)): (Int | Double) = ???
    def minus(a: (Int | Double), b: (Int | Double)): (Int | Double) = ???
    override val toString = "d3"

end Num

enum Expr[TList <: TypeList, A]:
  self =>

  def +(expr: Expr[TList, A])(implicit ev: A `IsElementOf` TList): Expr[TList, A] = Sum(self, expr)

  def -(expr: Expr[TList, A])(implicit ev: A `IsElementOf` TList): Expr[TList, A] = Minus(self, expr)

  case Value[TList <: TypeList, A](a: A)(using ev: A `IsElementOf` TList) extends Expr[TList, A]

  case Prod[TList <: TypeList, A, B](
      lhs: Expr[TList, A],
      rhs: Expr[TList, B],
    )(using
      evA: A `IsElementOf` TList,
      evB: B `IsElementOf` TList) extends Expr[TList, (A, B)]

  case Sum[TList <: TypeList, A](
      lhs: Expr[TList, A],
      rhs: Expr[TList, A],
    )(using val ev: A `IsElementOf` TList) extends Expr[TList, A]

  case Minus[TList <: TypeList, A](
      lhs: Expr[TList, A],
      rhs: Expr[TList, A],
    )(using val ev: A `IsElementOf` TList) extends Expr[TList, A]

object Expr:

  def lit[A, TList <: TypeList](a: A)(using ev: A `IsElementOf` TList): Expr[TList, A] =
    Expr.Value(a)

  def prod[A, B, TList <: TypeList](
      a: Expr[TList, A],
      b: Expr[TList, B],
    )(using
      evA: A `IsElementOf` TList,
      evB: B `IsElementOf` TList,
    ): Expr[TList, (A, B)] =
    Expr.Prod(a, b)

  def eval[As <: TypeList, A](
      expr: Expr[As, A]
    )(implicit ins: Instances[Num, As]
    ): A =
    expr match
      case Expr.Value(v) =>
        v
      case sum @ Expr.Sum(lhs, rhs) =>
        def add[T](a: T, b: T): Num[T] => T = _.sum(a, b)
        ins.withInstance(add(eval(sum.lhs), eval(sum.rhs)))(sum.ev)

      case prod @ Expr.Prod(lhs, rhs) =>
        // useless
        (eval(lhs), eval(rhs))

      case m @ Expr.Minus(lhs, rhs) =>
        def min[T](a: T, b: T): Num[T] => T = _.minus(a, b)
        ins.withInstance(min(eval(lhs), eval(rhs)))(m.ev)

end Expr

extension [A: Num, TList <: TypeList](v: A)
  inline def lit(using n: Num[A], ev: A `IsElementOf` TList): Expr[TList, A] = Expr.Value(v)

@main def app(): Unit = Program()

object Program:
  import Expr.*

  type AllowedTypes = Double :: Int :: End

  import Instances.*

  def apply(): Unit =
    val expr: Expr[AllowedTypes, Double] = 2.78.lit + 78.5.lit - 1.1.lit + 6.lit

    val res = eval(expr)
    println(res)

end Program

/*
https://youtu.be/jtxO7LJXW0Q?list=LL
https://github.com/zio/zio-constraintless/blob/master/examples/shared/src/main/scala/zio/constraintless/examples/Expr.scala
*/