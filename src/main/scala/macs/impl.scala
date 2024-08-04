package macs

//https://eed3si9n.com/10things/?s=03
object impl {

  import scala.quoted.*

  inline def isCaseClass[A]: Boolean = ${ isCaseClassImpl[A] }

  private def isCaseClassImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val sym = TypeRepr.of[A].typeSymbol
    Expr(sym.isClassDef && sym.flags.is(Flags.Case))
  }

  inline def inc(inline x: Int): Int = ${ incM('{ x }) }

  private def incM(x: Expr[Int])(using Quotes): Expr[Int] = {
    val rhs = Expr(x.valueOrAbort + 1)
    '{
      val x = $rhs
      x
    }
  }

  val makeList = [a] => (a: a) => List(a)
  val makeOption: [a] => a => Option[a] = [a] => (a: a) => Option(a)

}
