package patches

//https://mhammons.hashnode.dev/containers
//https://slides.com/olegnizhnik/scala-3#/5/3/0
//https://slides.com/olegnizhnik/profdata#/3/6

object Tips3:

  import quoted.*

  case class A(name: String, arg: Int)
  case class B(name: String, arg: Double)
  case class C(name: String, arg: Char)

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
