package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.AST._
import se.kth.cda.arc.syntaxtree.ASTUtils._
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.arc.syntaxtree._
import se.kth.cda.compiler.Utils.fix

object Transformer {

  implicit class ExprTransformer(val self: Expr) extends AnyVal {

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
    def toWeldFlatmap: Expr = {
      self.kind match {
        case Lambda(Vector(arcBuilder, _, arcElement), arcBody) =>
          val weldBuilder = arcBuilder.toWeld
          val weldElement = arcElement.toWeld
          val weldBody = fix[Expr, Expr] { f =>
            expr =>
              if (expr.ty.isArcType && expr.ty.isBuilderType) {
                expr.kind match {
                  case Merge(builder, value) => Merge(f(builder), value).toExpr(expr.ty.toWeld)
                  case If(cond, onTrue, onFalse) => If(cond, f(onTrue), f(onFalse)).toExpr(expr.ty.toWeld)
                  case Select(cond, onTrue, onFalse) => Select(cond, f(onTrue), f(onFalse)).toExpr(expr.ty.toWeld)
                  case For(iterator, builder, body) => For(iterator, builder, f(body)).toExpr(expr.ty.toWeld)
                  case Lambda(params, body) => Lambda(params.map(_.toWeld), f(body)).toExpr(expr.ty.toWeld)
                  case Let(symbol, bindingTy, value, body) => Let(symbol, bindingTy.toWeld, f(value), f(body)).toExpr(expr.ty.toWeld)
                  case _ => expr
                }
              } else {
                expr
              }
          }(arcBody)

          // Read this from down-to-up
          val bodySymbol = Symbol.unknown
          val result = Ident(bodySymbol).toExpr(weldBuilder.ty).toResult
          val letBody = Let(bodySymbol, weldBuilder.ty, weldBody, result).toExpr(result.ty)
          val letBuilder = Let(weldBuilder.symbol, weldBuilder.ty, weldBuilder.ty.toExpr, letBody).toExpr(letBody.ty)
          val weldExpr = Lambda(Vector(weldElement), letBuilder).toExpr(self.ty.toWeld)
          weldExpr
        case _ => ???
      }
    }

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

    def toResult: Expr = {
      self.ty match {
        case Struct(elemTys) =>
        case
      }
    }
  }

  implicit class ParameterTransformer(val self: Parameter) extends AnyVal {
    def toWeld: Parameter = Parameter(self.symbol, self.ty.toWeld)
  }

  implicit class TypeTransformer(val self: Type) extends AnyVal {
    def toWeld: Type = {
      fix[Type, Type] { f => {
        case StreamAppender(elemTy, annotations) => Appender(elemTy, annotations)
        case Function(params, returnTy) => Function(params.map(f), f(returnTy))
        case Struct(elemTys) => Struct(elemTys.map(f))
        case ty@_ => ty
      }
      }(self)
    }

    def toExpr: Expr = {
      fix[Type, Expr] { f => {
        case ty@Struct(elemTys) => MakeStruct(elemTys.map(f)).toExpr(ty)
        case ty@Appender(elemTy, annots) => NewBuilder(StreamAppender(elemTy, annots), Vector.empty).toExpr(ty)
        case _ => ???
      }
      }(self)
    }
  }

}