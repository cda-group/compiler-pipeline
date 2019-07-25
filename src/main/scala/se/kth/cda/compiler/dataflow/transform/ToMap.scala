package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind.Lambda

object ToMap {

  implicit class ToMap(val udf: Lambda) extends AnyVal {

    // A mapper is a body which contains only one merge(builder, value) expression
    // Therefore, we need to turn the body into a Weld function which just returns the value
    //
    // |b,e| merge(b, e+5)
    //
    // becomes
    //
    // |e| e+5
    //
    // A problem is if we have a function that binds the builder
    //
    // |b,i,e| let b1 = b; merge(b1, e+5)
    //
    // or
    //
    // |b,i,e| let b1 = merge(b, e+5); b1
    //
    // What do we do then?
    //
    def toWeldMap: Expr = {
      ???
      //val weldBody = fix[Expr, Expr] { f =>
      //  expr =>
      //    expr.ty match {
      //      case appender: StreamAppender =>
      //        expr.kind match {
      //          case Merge(_, value) => Ascription(value, value.ty).toExpr(appender.elemTy)
      //          case For(iterator, builder, body) => For(iterator,)
      //          case If(cond, onTrue, onFalse) => If(cond, f(onTrue), f(onFalse)).toExpr(appender.elemTy)
      //          case Select(cond, onTrue, onFalse) => Select(cond, f(onTrue), f(onFalse)).toExpr(appender.elemTy)
      //          case _ => ???
      //        }
      //      case _ => expr
      //    }
      //}(arcBody)
      //Lambda(Vector(arcEvent), weldBody).toExpr(weldBody.ty)
    }

  }

}
