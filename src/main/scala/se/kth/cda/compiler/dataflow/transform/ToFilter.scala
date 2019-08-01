package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.compiler.Utils.fix

object ToFilter {

  implicit class ToFilter(val udf: Lambda) extends AnyVal {

    def toFilter: Expr = {
      val Lambda(Vector(arcBuilder, _, arcElement), arcBody) = udf
      val weldElement = arcElement
      val weldBuilder = arcBuilder
      val weldBody = fix[(Expr, Map[String, Expr]), Expr] { f =>
        {
          case (e, c) =>
            e.ty match {
              case StreamAppender(elemTy, _) =>
                e.kind match {
                  // let b1 = b; body
                  case Let(symbol, bindingTy, value, body) =>
                    bindingTy match {
                      case StreamAppender(_, _) => f((body, c + (symbol.name -> f((value, c)))))
                      case _                    => Let(symbol, bindingTy, value, f((body, c))).toExpr(elemTy)
                    }
                  // b1
                  case Ident(sym) => if (sym.name == arcBuilder.symbol.name) { e } else { c(sym.name) }
                  // Ignore select for now
                  case Select(_, _, _) => ???
                  // if(cond, merge(b, elem), b)
                  case If(cond, onTrue, onFalse) =>
                    (onTrue.kind, onFalse.kind) match {
                      case (Merge(Expr(Ident(sym1), _, _, _), Expr(Ident(sym2), _, _, _)), Ident(sym3))
                          if sym1.name == weldBuilder.symbol.name &&
                            sym2.name == weldElement.symbol.name &&
                            sym3.name == weldBuilder.symbol.name =>
                        cond
                      case (Ident(sym1), Merge(Expr(Ident(sym2), _, _, _), Expr(Ident(sym3), _, _, _)))
                          if sym1.name == weldBuilder.symbol.name &&
                            sym2.name == weldBuilder.symbol.name &&
                            sym3.name == weldElement.symbol.name =>
                        Not(cond).toExpr(Bool)
                      case _ => ???
                    }
                  case _ => ???
                }
              case _ => e
            }
        }
      }((arcBody, Map.empty))
      Lambda(Vector(weldElement), weldBody).toExpr(Bool)
    }
  }
}
