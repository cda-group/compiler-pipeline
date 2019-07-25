package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.ExprKind.{Lambda, _}
import se.kth.cda.arc.syntaxtree.AST.{Expr, _}
import se.kth.cda.arc.syntaxtree.ASTUtils._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.compiler.Utils.fix
import se.kth.cda.compiler.dataflow.transform.Utils._

object ToFlatmap {
  implicit class ToFlatmap(val udf: ExprKind.Lambda) extends AnyVal {
    // Flatmaps are the most general types of combinators, which may contain any number of merge(builder, value)
    // where the builder type is a StreamAppender[T] or structs of StreamAppender[T]s
    //
    // To transform flatmaps into Weld code, we must
    // 1. Replace all type hints of StreamAppender[T] with Appender[T]
    // 2. Initialize and call result on the Appender, instead of taking it as a parameter
    // 3. Pack/unpack in cases where the Appender is instead a struct of Appenders
    //
    // For example, the Arc code:
    //
    // for(source, sink, |b:{StreamAppender[i32], StreamAppender[i32]}, e:i32|
    //     let b1: StreamAppender[i32] = merge(b.$0, e);
    //     let b2: StreamAppender[i32] = merge(b.$0, e);
    //     b2
    //  )
    //
    // Should generate:
    //
    // |e:i32|
    //     let b: {Appender[i32], Appender[i32]} = {Appender[i32], Appender[i32]};
    //     let final = (
    //         let b1: StreamAppender[i32] = merge(b.$0, e);
    //         let b: StreamAppender[i32] = merge(b.$0, e);
    //         b2
    //     );
    //     {result(final.$0), result(final.$1)}
    //
    // TODO: An edge which might be worth considering is when the positions of builders in struct are permuted
    // TODO: i.e. for(v,b,|b,i,e| {merge(b.$1, 1), merge(b.$0, 0)}
    //
    def toFlatmap: Expr = {
      val Lambda(Vector(arcBuilder, _, arcElement), arcBody) = udf
      val weldBuilder = arcBuilder.toWeld
      val weldElement = arcElement.toWeld
      val weldBody = fix[Expr, Expr] { f => expr =>
        if (expr.ty.isArcType && expr.ty.isBuilderType) {
          expr.kind match {
            case e: Merge  => Merge(f(e.builder), e.value).toExpr(expr.ty.toWeldType)
            case e: If     => If(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty.toWeldType)
            case e: Select => Select(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty.toWeldType)
            case e: For    => For(e.iterator, e.builder, f(e.body)).toExpr(expr.ty.toWeldType)
            case e: Lambda => Lambda(e.params.map(_.toWeld), f(e.body)).toExpr(expr.ty.toWeldType)
            case e: Let    => Let(e.symbol, e.bindingTy.toWeldType, f(e.value), f(e.body)).toExpr(expr.ty.toWeldType)
            case _         => expr
          }
        } else {
          expr
        }
      }(arcBody)
      // We need to construct this in reverse sadly
      val id = Ident(Symbol.unknown)
      val result = id.toExpr(weldBuilder.ty).toResult
      val letBody = Let(id.symbol, weldBuilder.ty, weldBody, result).toExpr(result.ty)
      val letBuilder = Let(weldBuilder.symbol, weldBuilder.ty, weldBuilder.ty.toWeldExpr, letBody).toExpr(letBody.ty)
      val weldExpr = Lambda(Vector(weldElement), letBuilder).toExpr(Function(Vector(weldElement.ty), result.ty))
      weldExpr
    }

  }

}
