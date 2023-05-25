// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package edsl2

// Typed AST, with two simple interpreters (pretty-printing and evaluation).
enum Exp[T]:
  case Num(value: Int) extends Exp[Int]
  case Bool(value: Boolean) extends Exp[Boolean]

  case Add(lhs: Exp[Int], rhs: Exp[Int]) extends Exp[Int]
  case Eq(lhs: Exp[Int], rhs: Exp[Int]) extends Exp[Boolean]

  /*
  case Add(lhs: Exp[T], rhs: Exp[T]) extends Exp[T]
  case Eq(lhs: Exp[T], rhs: Exp[T]) extends Exp[Boolean]
   */

  case Cond(
      cond: Exp[Boolean],
      ifTrue: Exp[T],
      ifFalse: Exp[T]) extends Exp[T]

  def pretty: String = this match
    case Num(value)                  => value.toString
    case Bool(value)                 => value.toString
    case Add(lhs, rhs)               => s"${lhs.pretty} + ${rhs.pretty}"
    case Eq(lhs, rhs)                => s"${lhs.pretty} = ${rhs.pretty}"
    case Cond(cond, ifTrue, ifFalse) => s"if ${cond.pretty} then ${ifTrue.pretty} else ${ifFalse.pretty}"

  def eval: T = this match
    case Num(value)                  => value
    case Bool(value)                 => value
    case Add(lhs, rhs)               => lhs.eval + rhs.eval
    case Eq(lhs, rhs)                => lhs.eval == rhs.eval
    case Cond(cond, ifTrue, ifFalse) => if cond.eval then ifTrue.eval else ifFalse.eval

// Untyped AST, with tools to type-check it.
enum UntypedExp:
  self =>

  case Num(value: Int)
  case Bool(value: Boolean)

  case Add(lhs: UntypedExp, rhs: UntypedExp)
  case Eq(lhs: UntypedExp, rhs: UntypedExp)

  case Cond(
      cond: UntypedExp,
      ifTrue: UntypedExp,
      ifFalse: UntypedExp)

  def typeCheck: TypedExp = self match
    // Literals.
    case Num(value)  => TypedExp.Num(Exp.Num(value))
    case Bool(value) => TypedExp.Bool(Exp.Bool(value))

    // Simple binary operations.
    case Add(lhs, rhs) =>
      (lhs.typeCheck, rhs.typeCheck) match
        case (TypedExp.Num(left), TypedExp.Num(right)) => TypedExp.Num(Exp.Add(left, right))
        case _                                         => TypedExp.Failure("Addition expects integer operands")

    case Eq(lhs, rhs) =>
      (lhs.typeCheck, rhs.typeCheck) match
        case (TypedExp.Num(left), TypedExp.Num(right)) => TypedExp.Bool(Exp.Eq(left, right))
        case _                                         => TypedExp.Failure("Numeric equality expects integer operands")

    // This one is trickier. We need to make sure the "true" and "false" branch evaluate to the same type tt, and yield
    // an Exp[tt].
    case Cond(cond, ifTrue, ifFalse) =>
      def unify(
          cond: Exp[Boolean],
          ifTrue: TypedExp,
          ifFalse: TypedExp,
        ): TypedExp =
        Unify(ifTrue, ifFalse) match
          case Unify.Success(left, right, tpe) => TypedExp(tpe, Exp.Cond(cond, left, right))
          case Unify.Failure(e)                => TypedExp.Failure(e)

      (cond.typeCheck, ifTrue.typeCheck, ifFalse.typeCheck) match
        case (TypedExp.Bool(cnd), left, right) => unify(cnd, left, right)
        case (TypedExp.Success(_, _), _, _)    => TypedExp.Failure(s"Conditions should be booleans")
        case _                                 => TypedExp.Failure("todo")

// Represents the possible types of an expression.
enum Type[T]:
  case Num extends Type[Int]
  case Bool extends Type[Boolean]

// Represents the result of type checking an untyped expression.
//
// Note how in the success case, both Type and Exp are indexed on the same T - this allows us to pattern match on
// something having a type of Type.Num, for example, and have the compiler understands that it *must* mean the
// corresponding expression is of type Exp[Int].
enum TypedExp:
  case Failure(error: String)
  case Success[T](tpe: Type[T], exp: Exp[T])

// Helper functions for creating / deconstructing TypedExp values.
//
// This is really only meant to make the type checking code a little less boilerplatey.
//
// Note that one could see these helpers and reason that we would be better off writing type-specific branches
// of TypedExp - one for Num, one for Bool, instead of a single Success one.
// It turns out, however, that being able to manipulate concrete types as values is very useful when handling Cond
// statements.
object TypedExp:
  def apply[T](tpe: Type[T], exp: Exp[T]): TypedExp = TypedExp.Success(tpe, exp)

  object Num:
    def apply(exp: Exp[Int]): TypedExp = TypedExp(Type.Num, exp)

    def unapply(exp: TypedExp): Option[Exp[Int]] = exp match
      case TypedExp.Success(Type.Num, exp) => Some(exp)
      case _                               => None

  object Bool:
    def apply(exp: Exp[Boolean]): TypedExp = TypedExp(Type.Bool, exp)

    def unapply(exp: TypedExp): Option[Exp[Boolean]] = exp match
      case TypedExp.Success(Type.Bool, exp) => Some(exp)
      case _                                => None

// Proof that two expressions are of the same static type, associated with the corresponding Type value.
//
// This is useful when we need two expressions to be of the same type: Unify.apply will either fail,
// or return something that'll allow the compiler to infer that both the lhs and rhs are of the same type.
enum Unify:
  case Success[
      A
    ](
      lhs: Exp[A], rhs: Exp[A], tpe: Type[A],
    )
  case Failure(error: String)

object Unify:
  def apply[A](
      lhs: Exp[A],
      rhs: Exp[A],
      tpe: Type[A],
    ): Unify = Unify.Success(lhs, rhs, tpe)

  // Attempts to prove that both lhs and rhs are of the same type by simply pattern matching on all known types.
  def apply(lhs: TypedExp, rhs: TypedExp): Unify = (lhs, rhs) match
    case (TypedExp.Num(left), TypedExp.Num(right))              => Unify(left, right, Type.Num)
    case (TypedExp.Bool(left), TypedExp.Bool(right))            => Unify(left, right, Type.Bool)
    case (TypedExp.Success(tpe1, _), TypedExp.Success(tpe2, _)) => Unify.Failure(s"Could not unify $tpe1 and $tpe2")
    case _                                                      => Unify.Failure("some error")

object Program:

  def apply(): Unit =
    // import Language.*
    import UntypedExp.*

    // This must fail, the branches of that Cond statement are of incompatible types.
    println(Cond(Bool(true), Bool(false), Num(1)))

    // This is valid (if 1 + 2 = 3 then 4 else 5).
    val exp = Cond(
      Eq(
        Add(Num(4), Num(2)),
        Num(3),
      ),
      Num(4),
      Num(5),
    )

    // Demonstrates that we get useful type information: in the TypedExp.Num branch, we know that our expression is
    // of type Exp[Int] (as evidenced by the fact that it evaluates into an int and this all compiles).
    exp.typeCheck match
      case TypedExp.Num(exp) =>
        println("Exp: " + exp.pretty)
        val i: Int = exp.eval
        println(s"out > $i")

      case _ =>
        println("Failed to type check")

  end apply

@main def main: Unit = Program()
