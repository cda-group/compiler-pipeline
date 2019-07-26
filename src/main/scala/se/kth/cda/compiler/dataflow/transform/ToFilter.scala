package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind.Lambda

object ToFilter {

  implicit class ToFilter(val udf: Lambda) extends AnyVal {

    def toFilter: Expr = {
      ???
    }
  }
}
