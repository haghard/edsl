package edsl5

import cats.syntax.all.*
import scala.util.Try
import cats.*


/*

https://scastie.scala-lang.org/MZrGFXDdQLyyodz9K8qeTQ

https://nrinaudo.github.io/articles/pl/typed_expr.html
https://nrinaudo.github.io/articles/pl/functions.html
https://nrinaudo.github.io/articles/pl/recursion.html
https://nrinaudo.github.io/articles/pl/type_checking.html

https://github.com/nrinaudo/lsp-demo/blob/main/server/src/main/scala/exp/Type.scala

https://youtu.be/WfRYEFuWx7I

*/

enum Type {
  case Num
  case Bool
  case Lambda[A <: Type, B <: Type](from: A, to: B)
}

// Type aliases to make things a little less unreadable.
object Type {
  type Num = Type.Num.type
  type Bool = Type.Bool.type
}

enum Expr[A <: Type] {

  case Bool(b: Boolean) extends Expr[Type.Bool]
  case Num(i: Int) extends Expr[Type.Num]
  case Equals(lhs: Expr[Type.Num], rhs: Expr[Type.Num]) extends Expr[Type.Bool]
  case Add(lhs: Expr[Type.Num], rhs: Expr[Type.Num]) extends Expr[Type.Num]

  case Cond[A <: Type](
      pred: Expr[Type.Bool],
      thenBranch: Expr[A],
      elseBranch: Expr[A]) extends Expr[A]

  case Let[A <: Type, B <: Type](
      name: String,
      value: Expr[A],
      body: Expr[B]) extends Expr[B]

  case LetRec[A <: Type, B <: Type, C <: Type](
      name: String,
      value: Expr[Type.Lambda[A, B]],
      body: Expr[C]
  ) extends Expr[C]

  case Var(name: String)

  case Lambda[A <: Type, B <: Type](param: String, body: Expr[B]) extends Expr[Type.Lambda[A, B]]

  case Apply[A <: Type, B <: Type](
      lambda: Expr[Type.Lambda[A, B]],
      arg: Expr[A]) extends Expr[B]

}

enum TypeRepr[A <: Type] {
  case Num extends TypeRepr[Type.Num]
  case Bool extends TypeRepr[Type.Bool]
  case Lambda[A <: Type, B <: Type](lhs: TypeRepr[A], rhs: TypeRepr[B]) extends TypeRepr[Type.Lambda[A, B]]
}

object TypeRepr {
  // This gives us a `TypeRepr` for some `Type`. We don't know what the type parameter is, just that it exists, which
  // is enough for our purposes.
  def from(tpe: Type): TypeRepr[?] =
    tpe match {
      case Type.Bool => TypeRepr.Bool
      case Type.Num  => TypeRepr.Num
      case Type.Lambda(a, b) =>
        TypeRepr.Lambda(TypeRepr.from(a), TypeRepr.from(b))
    }
}

// - Untyped expressions -----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Primitives of our language, without any form of type checking. It's for example perfectly valid to add integers and
// booleans. We fix that by compiling to a typed AST.
enum UntypedExpr {

  // Boolean and number literals.
  case Bool(b: Boolean)
  case Num(i: Int)

  // Simple operators, for test purposes.
  case Equals(lhs: UntypedExpr, rhs: UntypedExpr)
  case Add(lhs: UntypedExpr, rhs: UntypedExpr)

  // if pred then thenBranch else elseBranch
  case Cond(
      pred: UntypedExpr,
      thenBranch: UntypedExpr,
      elseBranch: UntypedExpr)

  // Variable assignment: let name = value in body.
  case Let(
      name: String,
      value: UntypedExpr,
      body: UntypedExpr)

  // Variable assignment in which `name` is a valid binding in `value`.
  // Think `let f = x -> f (x + 1) in f 0`.
  // We need to know the value's type for type checking.
  case LetRec(
      name: String,
      value: UntypedExpr,
      valueType: Type,
      body: UntypedExpr)

  // Variable lookup.
  case Var(name: String)

  // Function declaration: (x: Int) -> x + 1
  // We need to know the type of the parameter for type checking.
  case Lambda(
      param: String,
      paramType: Type,
      body: UntypedExpr)

  // Function application: f 1.
  case Apply(lambda: UntypedExpr, arg: UntypedExpr)
}

// Stores all type information we have about bindings. We can either bind a name to a new type, or lookup the type
// a name is bound to.
//
// This allows us to type check variable bindings and functions.
case class TypeEnv(map: Map[String, TypeRepr[?]]) {

  def bind[A <: Type](name: String, tpe: TypeRepr[A]) =
    TypeEnv(map + (name -> tpe))

  def lookup(name: String): Either[String, TypeRepr[?]] =
    map.get(name) match {
      case Some(tpe) => Right(tpe)
      case None      => Left(s"Type binding $name not found")
    }
}
object TypeEnv {
  val empty = TypeEnv(Map.empty)
}

/** Proof that 2 types are equal. */
enum Equality[A, B] {
  case Refl[A]() extends Equality[A, A]

  def andThen[C](eq: Equality[B, C]): Equality[A, C] = (this, eq) match { case (Refl(), Refl()) => Refl() }

  def cast(a: A): B = this match {
    case Refl() => a
  }
}

object Equality {

  def injectivity[F[_ <: Type], A <: Type, B <: Type](
      eq: Equality[A, B]
    ): Equality[F[A], F[B]] = eq match {
    case Refl() => Refl()
  }

  def lambda[LA <: Type, LB <: Type, RA <: Type, RB <: Type](
      eqA: Equality[LA, RA],
      eqB: Equality[LB, RB],
    ): (Equality[Type.Lambda[LA, LB], Type.Lambda[RA, RB]]) = {
    // Eq[LA, RA] allows us to get the left hand side of our desired equality.
    val step1: Equality[Type.Lambda[LA, LB], Type.Lambda[RA, LB]] =
      Equality.injectivity[[X <: Type] =>> Type.Lambda[X, LB], LA, RA](eqA)

    // Eq[LB, RB] allows us to get the left hand side of our desired equality.
    val step2: Equality[Type.Lambda[RA, LB], Type.Lambda[RA, RB]] =
      Equality.injectivity[[X <: Type] =>> Type.Lambda[RA, X], LB, RB](eqB)

    // If you look at the types, we have the left hand side equal to some odd type, which is itself equal to the
    // right hand side. We can get our desired equality by transitivity of type equalities.
    step1.andThen(step2)
  }

  // That's the ultimate goal: produce type equalities for `Expr` when we have them for `TypeRepr`. The magic happens
  // below in `repr` however.
  def expr[A <: Type, B <: Type](
      lhs: TypeRepr[A],
      rhs: TypeRepr[B],
    ): Either[String, Equality[Expr[A], Expr[B]]] = repr(lhs, rhs).map(Equality.injectivity)

  // Attempts to prove type equality of `A` and `B` when we have the corresponding `TypeRepr`.
  // This is done by an exhaustive pattern match on the `TypeRepr`, which allows the compiler to see when `A` and `B`
  // are in fact the same type. The lambda case is a little more complicated, but is really just recursing on the
  // components and gluing them back together in `lambda`.
  def repr[A <: Type, B <: Type](
      lhs: TypeRepr[A],
      rhs: TypeRepr[B],
    ): Either[String, Equality[A, B]] =
    (lhs, rhs) match {
      case (TypeRepr.Num, TypeRepr.Num)   => Right(Refl())
      case (TypeRepr.Bool, TypeRepr.Bool) => Right(Refl())
      case (TypeRepr.Lambda(lf, lt), TypeRepr.Lambda(rf, rt)) =>
        for {
          from <- repr(lf, rf)
          to <- repr(lt, rt)
        } yield lambda(from, to)

      case _ => Left(s"Failed to prove $lhs = $rhs")
    }
}

enum Typing {
  case Result[A <: Type](expr: Expr[A], repr: TypeRepr[A])

  // Attempts to cast this `Typing` to the specified type. This is the magic bit.
  def cast[A <: Type](to: TypeRepr[A]): Either[String, Expr[A]] = this match {
    case Result(expr, from) =>
      Equality.expr(from, to).map(_.cast(expr))
  }
}

import Typing.*

// Simple convenience so that we can call `cast` without having to unwrap `Either`.
extension (expr: Either[String, Typing]) {
  def cast[A <: Type](to: TypeRepr[A]): Either[String, Expr[A]] =
    expr.flatMap(_.cast(to))
}

enum Interpreted[A <: Type] {
  case Num(i: Int) extends Interpreted[Type.Num]
  case Bool(b: Boolean) extends Interpreted[Type.Bool]
  case Lambda[A <: Type, B <: Type](f: Interpreted[A] => Interpreted[B]) extends Interpreted[Type.Lambda[A, B]]
}

// Stores all bindings
// This is currently implemented using runtime casts, which I think is acceptable: the existence of an `Expr[A]` proves
// the types are correct, any error here would be a critical bug that should lead to a core dump.
// I'm pretty confident I will eventually rewrite this to use, I suspect, GADTs, to make this fully safe.
final case class Env(
  bindings: collection.mutable.Map[String, Interpreted[?]]
) {

  def bind[A <: Type](name: String, value: Interpreted[A]) =
    // map + (name -> value)
    Env(bindings.+=(name -> value))

  def set[A <: Type](name: String, value: Interpreted[A]) =
    bindings.+=(name -> value)

  def lookup[A <: Type](name: String) =
    bindings.get(name) match {
      case Some(a) =>
        Try(a.asInstanceOf[Interpreted[A]])
          .getOrElse(sys.error(s"Bad type for $name"))
      case None =>
        sys.error(s"Unbound variable: $name")
    }
}

object Env {
  val empty: Env = Env(collection.mutable.Map.empty)
}

object Program {

  def interpret[A <: Type](expr: Expr[A], env: Env): Interpreted[A] =
    expr match {
      case Expr.Bool(b) =>
        Interpreted.Bool(b)
      case Expr.Num(i)  =>
        Interpreted.Num(i)

      // Simple operations are merely interpreting the operands and performing the underlying operation on them.
      case Expr.Add(lhs, rhs) =>
        (interpret(lhs, env), interpret(rhs, env)) match {
          case (Interpreted.Num(lhs), Interpreted.Num(rhs)) =>
            println(s"$lhs + $rhs = ${lhs + rhs}")
            Interpreted.Num(lhs + rhs)
        }

      case Expr.Equals(lhs, rhs) =>
        (interpret(lhs, env), interpret(rhs, env)) match {
          case (Interpreted.Num(lhs), Interpreted.Num(rhs)) =>
            Interpreted.Bool(lhs == rhs)
        }

      // If the predicate evaluates to true, evaluate the `thenBranch`. Otherwise, evaluate the `elseBranch`.
      case Expr.Cond(p, t, e) =>
        interpret(p, env) match {
          case Interpreted.Bool(true)  => interpret(t, env)
          case Interpreted.Bool(false) => interpret(e, env)
        }

      // Simply evaluates `body` in an environment that binds `name` to `value`.
      case Expr.Let(name, value, body) =>
        interpret(body, env.bind(name, interpret(value, env)))

      // This is slightly more convoluted since we must interpret `value` in an environment in which it's already bound to
      // `name`. The trick is that we get to create the binding now, but set it before `value` is actually used.
      case Expr.LetRec(name, value, body) =>
        val newEnv = env.bind(name, null)
        interpret(value, newEnv) match {
          case value: Interpreted.Lambda[a, b] =>
            newEnv.set(name, value)
            interpret(body, newEnv)
        }

      case Expr.Var(name) =>
        println("lookup: " + name)
        env.lookup(name)

      // Note how this captures the environment in which the lambda was defined, as it should.
      case lambda: Expr.Lambda[a, b] =>
        Interpreted.Lambda[a, b](a => interpret(lambda.body, env.bind(lambda.param, a)))

      // Simply gets the lambda, then applies it to its argument.
      case Expr.Apply(lambda, arg) =>
        interpret(lambda, env) match {
          case Interpreted.Lambda(f) =>
            f(interpret(arg, env))
        }
    }

  def showBool(b: Boolean) = b.toString
  def showInt(i: Int) = i.toString
  def showLambda[A, B](f: A => B) = "<function>"

  // Shows _any_ typed expression.
  def show(expr: Typing): String =
    expr match {
      case Result(expr, TypeRepr.Bool) =>
        interpret(expr, Env.empty) match {
          case Interpreted.Bool(b) =>
            val r = showBool(b)
            println("showBool: " + r)
            r
        }

      case Result(expr, TypeRepr.Num) =>
        interpret(expr, Env.empty) match {
          case Interpreted.Num(i) =>
            val r = showInt(i)
            println("showInt: " + r)
            r
        }

      case Result(expr, TypeRepr.Lambda(_, _)) =>
        interpret(expr, Env.empty) match {
          case Interpreted.Lambda(f) =>
            val r = showLambda(f)
            println("showLambda:  " + r)
            r
        }
    }

  def checkBool(b: Boolean) =
    Right(Result(Expr.Bool(b), TypeRepr.Bool))

  // Γ |- Num i : Type.Num
  def checkNum(i: Int) =
    Right(Result(Expr.Num(i), TypeRepr.Num))

  // Γ |- lhs: Type.Num, Γ |- rhs: Type.Num
  // --------------------------------------
  // Γ |- Add lhs rhs : Type.Num
  def checkAdd(
      lhs: UntypedExpr,
      rhs: UntypedExpr,
      env: TypeEnv,
    ) =
    (typeCheck(lhs, env), typeCheck(rhs, env)).mapN {
      case (Result(lhs, TypeRepr.Num), Result(rhs, TypeRepr.Num)) =>
        Result(Expr.Add(lhs, rhs), TypeRepr.Num)
    }

  // Γ |- lhs: Type.Num, Γ |- rhs: Type.Num
  // --------------------------------------
  // Γ |- Eq lhs rhs : Type.Bool
  def checkEq(
      lhs: UntypedExpr,
      rhs: UntypedExpr,
      env: TypeEnv,
    ) =
    (typeCheck(lhs, env), typeCheck(rhs, env)).mapN {
      case (Result(lhs, TypeRepr.Num), Result(rhs, TypeRepr.Num)) =>
        Result(Expr.Equals(lhs, rhs), TypeRepr.Bool)
    }

  // Γ |- Var name: Γ(name)
  def checkVar(name: String, env: TypeEnv) =
    env.lookup(name).map(tpe => Result(Expr.Var(name), tpe))

  // Γ |- p: Type.Bool, Γ |- t: X , Γ |- e: X
  // ----------------------------------------
  // Γ |- Cond p t e : X
  def checkCond(
      p: UntypedExpr,
      t: UntypedExpr,
      e: UntypedExpr,
      env: TypeEnv,
    ) =
    typeCheck(p, env).cast(TypeRepr.Bool).flatMap { p =>
      (typeCheck(t, env), typeCheck(e, env)).flatMapN {
        case (t, Result(e, tpe)) =>
          t.cast(tpe).map { t =>
            Result(Expr.Cond(p, t, e), tpe)
          }
      }
    }

  // Γ |- value: T , Γ[name <- T] |- body: X
  // ----------------------------------------
  // Γ |- Let name value body : X
  def checkLet(
      name: String,
      value: UntypedExpr,
      body: UntypedExpr,
      env: TypeEnv,
    ) =
    typeCheck(value, env).flatMap {
      case Result(value, valueType) =>
        typeCheck(body, env.bind(name, valueType)).map {
          case Result(body, bodyType) =>
            Result(Expr.Let(name, value, body), bodyType)
        }
    }

  // Γ(name <- valueType) |- value: valueType , Γ[name <- valueType] |- body: X
  // ----------------------------------------------------------------------------
  // Γ |- LetRec name (value: valueType) body : X
  def checkLetRec(
      name: String,
      value: UntypedExpr,
      valueType: Type,
      body: UntypedExpr,
      env: TypeEnv,
    ) = {

    val newEnv = env.bind(name, TypeRepr.from(valueType))

    typeCheck(value, newEnv).flatMap {
      case Result(value, TypeRepr.Lambda(_, _)) =>
        typeCheck(body, newEnv).map {
          case Result(body, bodyType) =>
            Result(Expr.LetRec(name, value, body), bodyType)
        }
      case Result(_, other) =>
        Left(s"Expected a lambda but got $other")
    }
  }

  // Γ(param <- paramType) |- body: X
  // -----------------------------------------------------
  // Γ |- Lambda (param: paramType) body: Type.Lambda[paramType, X]
  def checkLambda(
      param: String,
      paramType: Type,
      body: UntypedExpr,
      env: TypeEnv,
    ) = {
    val paramRepr = TypeRepr.from(paramType)
    typeCheck(body, env.bind(param, paramRepr)).map {
      case Result(body, bodyRepr) =>
        Result(Expr.Lambda(param, body), TypeRepr.Lambda(paramRepr, bodyRepr))
    }
  }

  def checkApply(
      lambda: UntypedExpr,
      arg: UntypedExpr,
      env: TypeEnv,
    ) =
    typeCheck(lambda, env).flatMap {
      case Result(lambda, TypeRepr.Lambda(from, to)) =>
        typeCheck(arg, env).cast(from).map { arg =>
          Result(Expr.Apply(lambda, arg), to)
        }
      case Result(_, other) => Left(s"Expected a lambda but got $other")
    }


  /*
    val lower = 0; val upper = 15
    (0 to 15).map { i => lower + sum(lower + 1, upper) }
  */

  /*
    fun sumFor lower upper =
        var acc = 0
        for i = lower to upper do
          acc = acc + i
        acc
   */

  def typeCheck(expr: UntypedExpr, env: TypeEnv): Either[String, Typing] =
    expr match {
      case UntypedExpr.Bool(b)          =>
        checkBool(b)
      case UntypedExpr.Num(i)           =>
        checkNum(i)
      case UntypedExpr.Equals(lhs, rhs) =>
        checkEq(lhs, rhs, env)
      case UntypedExpr.Add(lhs, rhs)    =>
        checkAdd(lhs, rhs, env)
      case UntypedExpr.Var(name)        =>
        checkVar(name, env)
      case UntypedExpr.Cond(p, t, e) =>
        checkCond(p, t, e, env)
      case UntypedExpr.Let(name, value, body) =>
        checkLet(name, value, body, env)
      case UntypedExpr.LetRec(name, value, valueType, body) =>
        checkLetRec(name, value, valueType, body, env)
      case UntypedExpr.Lambda(param, paramType, body) =>
        checkLambda(param, paramType, body, env)
      case UntypedExpr.Apply(lambda, arg) =>
        checkApply(lambda, arg, env)
    }

  import UntypedExpr.*

  def apply(): Unit = {

   /*
      let rec sum = (lower: Num) -> (upper: Num) ->
        if lower > upper then 0
        else lower + (sum(lower + 1) upper)

      in sum(1, 10)
   */
    val expr: UntypedExpr =
      LetRec(
        name = "sum",
        valueType = Type.Lambda(Type.Num, Type.Lambda(Type.Num, Type.Num)),
        value = Lambda(
          param = "lower",
          paramType = Type.Num,
          body = Lambda(
            param = "upper",
            paramType = Type.Num,
            body = Cond(
              pred = Equals(Var("lower"), Var("upper")),
              thenBranch = Var("lower"),
              elseBranch = Add(
                lhs = Var("lower"),
                rhs = Apply(
                  Apply(Var("sum"), Add(Var("lower"), Num(1))),
                  Var("upper"),
                ),
              ),
            ),
          ),
        ),
        body = Apply(Apply(Var("sum"), Num(0)), Num(15)),
      )

    // This should print 55, the sum of all numbers between 0 and 10.
    val result = typeCheck(expr, TypeEnv.empty)
    println(result)

    // result.map(interpret(_, Env.empty))

    // result.map { r => show(r) }

    println(result.map(show))
  }
}

@main def main: Unit = Program()
