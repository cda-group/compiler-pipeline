package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.ASTUtils._
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.arc.syntaxtree.Type.Builder.{StreamAppender, Windower}
import se.kth.cda.arc.syntaxtree.Type.Struct
import se.kth.cda.compiler.Utils.fix

object Analyzer {
  def selectivity(udf: Lambda): Float = {
    fix[Expr, Float] { f => expr =>
      if (expr.ty.isArcType && expr.ty.isBuilderType) {
        expr.kind match {
          case e: Merge       => 1 + f(e.builder)
          case _: For         => Float.MaxValue
          case e: If          => (f(e.onTrue) + f(e.onFalse)) / 2
          case e: Select      => (f(e.onTrue) + f(e.onFalse)) / 2
          case e: Let         => f(e.value) + f(e.body)
          case e: Application => e.args.map(f).sum + f(e.expr)
          case e: Lambda      => f(e.body)
          case e: MakeStruct  => e.elems.map(f).sum
          case _              => 0
        }
      } else {
        0
      }
    }(udf.body)
  }

  def fan_out(udf: Lambda): Int = {
    fix[Type, Int] { f =>
      {
        case _: StreamAppender => 1
        case _: Windower       => 1
        case e: Struct         => e.elemTys.map(f).sum
        case _                 => 0
      }
    }(udf.body.ty)
  }
}
