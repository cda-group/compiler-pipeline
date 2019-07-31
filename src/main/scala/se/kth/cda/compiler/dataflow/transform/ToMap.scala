package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.compiler.Utils.fix
import se.kth.cda.compiler.dataflow.transform.Utils._

object ToMap {

  implicit class ToMap(val udf: Lambda) extends AnyVal {

    // A mapper is a body which contains only one merge(builder, value) expression
    // Therefore, we need to turn the body into a Weld function which just returns the value
    //
    // |b,e|
    //   let v = 3;
    //   let b1 = b;
    //   let b2 = merge(b1, v+e+5);
    //   b2
    //
    // becomes
    //
    // |e|
    //   let b2 = 3+e+5;
    //   b2
    //
    // We need to inline the builder expression
    // By only merging once, we can inline the surrounding computation to the value of the merge
    //

    def toMap: Expr = {
      val Lambda(Vector(arcBuilder, _, arcElement), arcBody) = udf
      val weldElement = arcElement.toAppender
      val weldBody = fix[(Expr, Map[String, Expr]), Expr] { f =>
        {
          case (e, c) =>
            e.ty match {
              case StreamAppender(elemTy, _) =>
                e.kind match {
                  case Merge(_, value)               => f((value, c))
                  case Ident(sym)                    => if (sym.name == arcBuilder.symbol.name) { e } else { c(sym.name) }
                  case If(cond, onTrue, onFalse)     => If(cond, f((onTrue, c)), f((onFalse, c))).toExpr(elemTy)
                  case Select(cond, onTrue, onFalse) => Select(cond, f((onTrue, c)), f((onFalse, c))).toExpr(elemTy)
                  case Let(symbol, bindingTy, value, body) =>
                    bindingTy match {
                      case StreamAppender(_, _) => f((body, c + (symbol.name -> f((value, c)))))
                      case _                    => Let(symbol, bindingTy, value, f((body, c))).toExpr(elemTy)
                    }
                  case _ => ???
                }
              case _ => e
            }
        }
      }((arcBody, Map.empty))
      Lambda(Vector(weldElement), weldBody).toExpr(weldBody.ty)
    }

  }

}
