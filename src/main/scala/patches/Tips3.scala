// Copyright (c) 2021-23 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package patches

//https://mhammons.hashnode.dev/containers
//https://slides.com/olegnizhnik/scala-3#/5/3/0
//https://slides.com/olegnizhnik/profdata#/3/6

//https://yadukrishnan.live/compile-time-error-generation-using-inline-in-scala-3
object InlineCompilerError:

  import scala.compiletime.*
  import scala.compiletime.ops.string.*
  import ops.string.*

  inline def checkVersion(versionNo: String) =
    inline if (!constValue[Matches[versionNo.type, "[\\d]+\\.[\\d]+[\\.\\d]*"]])
      error("Invalid semantic version number format. Value of versionNo provided is " + codeOf(versionNo))
    else
      println(s"Correct version information")

  checkVersion("1.2.0.6")

//Type-level parser
//
object Parser:
  // Compile-time operations
  import scala.compiletime.ops.int.+
  import scala.compiletime.ops.string.{ CharAt, Length, Substring, Matches } // try Matches

  // parameter untupling,
  type Args[S <: String] <: Tuple = S match
    case "" => EmptyTuple
    case _ =>
      CharAt[S, 0] match
        case '%' =>
          CharAt[S, 1] match
            case 'd' => Int *: Args[Substring[S, 2, Length[S]]]
            case 's' => String *: Args[Substring[S, 2, Length[S]]]
        case _ => Args[Substring[S, 1, Length[S]]]

  def parse(str: String)(args: Args[str.type]): Unit =
    val list = args.toList
    println(s"${list(0)} is ${list(1)}")

  parse("%s is %d")("Adam Bell", 45)

  summon[Args["%s is %d"] =:= (String, Int)]

end Parser

object Terms:

  sealed trait TermOps[T]
  object Empty extends TermOps[Nothing]

  trait FilterableOps[T] extends TermOps[T]:
    def eq$(v: T): Unit
    def notEq$(v: T): Unit

  trait SetOps[T] extends TermOps[T]:
    def in$(vs: Set[T]): Unit

  trait TypedTermOps[T <: String & Singleton]:
    type A
    type Term <: TermOps[A]
    def ops: Term

  object TypedTermOps:

    given name: TypedTermOps["name"] with
      type A = String
      type Term = FilterableOps[A]
      def ops: Term = new FilterableOps[A]:
        def eq$(v: A): Unit = println(s"$v eq")
        def notEq$(v: A): Unit = println(s"$v notEq")

    given age: TypedTermOps["age"] with
      type A = Int
      type Term = SetOps[Int]
      def ops: Term = (vs: Set[Int]) => println(s"$vs in")

  end TypedTermOps

  def term(name: String)(using TTC: TypedTermOps[name.type]): TTC.Term = TTC.ops

  term("name").eq$("aa")
  term("name").notEq$("aa")
  term("age").in$(Set(1, 2))

end Terms

object Tips3:

  import quoted.*

  final case class A(name: String, arg: Int)
  final case class B(name: String, arg: Double)
  final case class C(name: String, arg: Char)

  trait Trm[A]:
    type T
    def term(a: A): T

  object Trm:
    extension [A](a: A)(using s: Trm[A]) def term = s.term(a)

  /*trait Show[A]:
    def show(a: A): String

  object Show:
    extension [A](a: A)(using s: Show[A]) def show = s.show(a)

  end Show
   */

  object A:
    // given Show[A] with def show(a: A): String = a.name
    given Trm[A] with
      type T = Int
      def term(a: A): T = a.arg

  object B:
    // given Show[B] with def show(b: B): String = b.name
    given Trm[B] with
      type T = Double
      def term(b: B): T = b.arg

  object C:
    // given Show[C] with def show(c: C): String = c.name
    given Trm[C] with
      type T = Char
      def term(c: C): T = c.arg

  trait Column[A[_]]:
    type B
    val b: B
    given ev: A[B]
    // The evidence of the type class Show|Trm is provided by the context function A[B] ?=> B => C.
    def use[C](f: A[B] ?=> B => C): C = f(b)

  object Column:
    inline def apply[A[_]](a: Any) = ${
      applyImpl[A]('a)
    }

    private def applyImpl[A[_]](a: Expr[Any])(using Quotes, Type[A]) =
      import quotes.reflect.*
      a match
        case '{ $a: i } =>
          TypeRepr.of[i].widen.asType match
            case '[j] =>
              val expr = Expr
                .summon[A[j]]
                .getOrElse(report.errorAndAbort(s"Can't find instance of ${Type.show[A[j]]} for ${Type.show[j]}"))
              '{
                new Column[A]:
                  type B = j
                  val b: B = ${ a.asExprOf[j] }
                  given ev: A[j] = ${ expr }
              }
  end Column

end Tips3
