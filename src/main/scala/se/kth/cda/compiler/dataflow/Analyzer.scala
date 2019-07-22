package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.ASTUtils._
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.arc.syntaxtree.Type.Builder.{StreamAppender, Windower}
import se.kth.cda.arc.syntaxtree.Type.Struct
import se.kth.cda.compiler.Utils.fix

object Analyzer {
  def selectivity(lambda: Lambda): Float = {
    fix[Expr, Float] { f =>
      expr =>
        if (expr.ty.isArcType && expr.ty.isBuilderType) expr.kind match {
          case Merge(builder, _) => 1 + f(builder)
          case For(_, _, _) => Float.MaxValue
          case If(_, onTrue, onFalse) => (f(onTrue) + f(onFalse)) / 2
          case Select(_, onTrue, onFalse) => (f(onTrue) + f(onFalse)) / 2
          case Let(_, _, value, body) => f(value) + f(body)
          case Application(func, args) => args.map(f).sum + f(func)
          case Lambda(_, body) => f(body)
          case MakeStruct(elems) => elems.map(f).sum
          case _ => 0
        } else {
          0
        }
    }(lambda.body)
  }

  def fan_out(lambda: Lambda): Int = {
    fix[Type, Int] { f => {
      case StreamAppender(_, _) => 1
      case Windower(_, _, _, _, _) => 1
      case _ => 0
      case Struct(elemTys) => elemTys.map(f).sum
      case _ => 0
    }
    }(lambda.body.ty)
  }


}
