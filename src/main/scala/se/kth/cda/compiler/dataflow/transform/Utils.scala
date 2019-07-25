package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.AST._
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.compiler.Utils._

object Utils {

  // Turns a struct of builders into a struct of values
  //     let foo = {streamappender[i32], streamappender[i32]};
  //  => let bar = {result(foo.$0), result(foo.$1)};
  implicit class ExprTransformer(val self: Expr) extends AnyVal {
    def toResult: Expr = {
      self.ty match {
        // Struct of builders
        case Struct(elemTys) =>
          val projections = elemTys.zipWithIndex.map {
            case (builder: Builder, index) => Projection(self, index).toExpr(builder).toResult
            case _                         => ???
          }
          MakeStruct(projections).toExpr(Struct(projections.map(_.ty)))
        // Single builder
        case builder: Builder =>
          Result(self).toExpr(builder.resultType)
        case _ => ???
      }
    }
  }

  // Turns an Arc parameter into a Weld parameter
  //     |source: streamappender, elem: i32|
  //  => |source: appender, elem: i32|
  implicit class ParameterTransformer(val self: Parameter) extends AnyVal {
    def toWeld: Parameter = Parameter(self.symbol, self.ty.toWeldType)
  }

  // Turns an Arc type into a Weld type
  //     streamappender
  //  => appender
  implicit class TypeTransformer(val self: Type) extends AnyVal {
    // Transforms an Arc type into a Weld type
    def toWeldType: Type = {
      fix[Type, Type] { f =>
        {
          case ty: StreamAppender => Appender(ty.elemTy, ty.annotations)
          case ty: Function       => Function(ty.params.map(f), f(ty.returnTy))
          case ty: Struct         => Struct(ty.elemTys.map(f))
          case ty @ _             => ty
        }
      }(self)
    }

    // Returns an instance of a Weld builder-type
    def toWeldExpr: Expr = {
      fix[Type, Expr] { f =>
        {
          case ty: Struct   => MakeStruct(ty.elemTys.map(f)).toExpr(ty)
          case ty: Appender => NewBuilder(Appender(ty.elemTy, ty.annotations), Vector.empty).toExpr(ty)
          case _            => ???
        }
      }(self)
    }
  }

}
