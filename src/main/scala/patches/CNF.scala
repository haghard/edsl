package patches

import scala.quoted.*

//https://github.com/jchyb/scala-metaprogramming-blogpost/blob/main/ex2-scala3/src/main/scala/Main.scala
//https://virtuslab.com/blog/metaprogramming-in-scala-2-3/

//CNF (Conjunctive Normal Form)
object CNF:

  transparent inline def apply(
      inline cnfSpec: String,
      inline booleans: Boolean*
    ): Boolean = ${ cnfImpl('cnfSpec, 'booleans) }

  def cnfImpl(
      using Quotes
    )(
      cnfSpec: Expr[String],
      booleanSeqExpr: Expr[Seq[Boolean]],
    ): Expr[Boolean] =

    // "unpack" Expr[Seq[T]] to Seq[Expr[T]]
    val Varargs(booleans: Seq[Expr[Boolean]]) = booleanSeqExpr: @unchecked

    // parse cnf string
    final case class Literal(negated: Boolean, refNum: Int)
    val cnfStr = cnfSpec.valueOrAbort
    val clauseStrs = cnfStr.split('^')
    val clauses: Array[Array[Literal]] = clauseStrs
      .map(num => num.substring(1, num.size - 1))
      .map(
        _.split('v').map(literalStr =>
          if (literalStr.startsWith("-")) Literal(true, literalStr.substring(1).toInt)
          else Literal(false, literalStr.toInt)
        )
      )

    // finally, map the data to code via quoting and splicing
    clauses
      .map { clause =>
        clause.foldLeft('{ false }) { (l: Expr[Boolean], literal: Literal) =>
          println(literal)
          if (literal.negated) '{ $l || !${ booleans(literal.refNum) } } // Expr[Boolean]
          else '{ $l || ${ booleans(literal.refNum) } } // Expr[Boolean]
        }
      }
      .foldLeft('{ true }) { (l: Expr[Boolean], r: Expr[Boolean]) =>
        val out = '{ $l && $r } // Expr[Boolean]
        println(out)
        out
      }
