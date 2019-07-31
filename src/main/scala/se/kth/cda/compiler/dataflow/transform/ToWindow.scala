package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.BinOpKind.Div
import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.compiler.Utils._
import se.kth.cda.compiler.dataflow.WindowAssigner
import se.kth.cda.compiler.dataflow.WindowAssigner.Tumbling
import se.kth.cda.compiler.dataflow.transform.Utils._

object ToWindow {

  implicit class ToWindow(val udf: Lambda) extends AnyVal {

    // A minute-wise tumbling window is defined as
    //
    // for(source,
    //  windower[Unit, merger[i32,+], i32, i32](
    //   |ts: u32, windows: vec[u32], state| { [ ts / 60L ] , () },
    //   |wm: u32, windows: vec[u32], state| { filter(windows, |ts| ts < wm / 60L), () },
    //   |id: u32, agg: merger[i32,+]|       { id, result(agg) },
    //  ),
    //  |w: windower[Unit, merger[i32,+], i32, i32], e: i32| merge(w, e)
    // )
    def toAssigner: WindowAssigner = {
      toTumbling.get
    }

    // TODO: Really stupid implementation for now
    // The window is tumbling with length N if the assigner is
    //
    //   [ts/N]
    //
    // Example: Length 3
    //
    //   0 1 2 3 4 5 6 7 8 9
    // 0 x x x
    // 1       x x x
    // 2             x x x
    //
    // 0/3 => 0
    // 1/3 => 0
    // 2/3 => 0
    // 3/3 => 1
    // 4/3 => 1
    // 5/3 => 1
    // 6/3 => 2
    //
    private def toTumbling: Option[Tumbling] = {
      val ts = udf.params(0).symbol
      println(s"$udf")
      val length = fix[(Expr, Map[String, Long]), Option[Long]] { f =>
        {
          case (e, c) =>
            e.kind match {
              case MakeStruct(Vector(windows, _)) =>
                f((windows, c))
              // let length = 5; body
              case Let(symbol, _, Expr(Literal.I64(_, value), _, _, _), body) =>
                f((body, c + (symbol.name -> value)))
              // [ts / length]
              // [ts / 5]
              case MakeVec(Vector(Expr(BinOp(Div, lhs, rhs), _, _, _))) =>
                println(s"$lhs $rhs")
                lhs.kind match {
                  case Ident(lhs_symbol) if lhs_symbol.name == ts.name =>
                    rhs.kind match {
                      case Ident(rhs_symbol)     => c.get(rhs_symbol.name)
                      case Literal.I64(_, value) => Some(value)
                      case _                     => None
                    }
                  case _ => None
                }
              case _ => None
            }
        }
      }((udf.body, Map.empty))

      length.map(Tumbling)
    }

    // The window is sliding with length N and stride S if the assigner is
    //
    //   for(rangeiter((ts-N+2))/S, ts/S, 1), appender, |b,_,e| merge(b, e));
    //
    // Slide 2, length 3
    //
    //   0 1 2 3 4 5 6 7 8 9
    // 0 x x x
    // 1     x x x
    // 2         x x x
    // 3             x x x
    // 4                 x x x
    //
    // (t-n+2)/s   t/s
    // (0-3+2)/2 - 0/2 => *-0
    // (1-3+2)/2 - 1/2 => 0-0
    // (2-3+2)/2 - 2/2 => 0-1
    // (3-3+2)/2 - 3/2 => 1-1
    // (4-3+2)/2 - 4/2 => 1-2
    // (5-3+2)/2 - 5/2 => 1-2
    // (6-3+2)/2 - 6/2 => 2-3
    //

    // TODO: Not sure what to put here
    // def toTrigger: Expr = { ??? }

    // TODO: This is probably not needed
    // def toLower: Expr = { ??? }

    // The Lift function is the first function executed by windows
    // All we need to do is change the windower type to its aggregation type
    def toLift: Expr = {
      val Lambda(Vector(arcBuilder, _, arcElement), arcBody) = udf
      val weldBuilder = arcBuilder.toAggregator
      val weldElement = arcElement
      val weldBody = fix[Expr, Expr] { f => expr =>
        expr.ty match {
          case _: Windower =>
            expr.kind match {
              case e: Merge  => Merge(f(e.builder), e.value).toExpr(expr.ty.toAggregator)
              case e: If     => If(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty.toAggregator)
              case e: Select => Select(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty.toAggregator)
              case e: For    => For(e.iterator, e.builder, f(e.body)).toExpr(expr.ty.toAggregator)
              case e: Lambda => Lambda(e.params.map(_.toAppender), f(e.body)).toExpr(expr.ty.toAggregator)
              case e: Let    => Let(e.symbol, e.bindingTy.toAggregator, f(e.value), f(e.body)).toExpr(expr.ty.toAggregator)
              case _         => expr
            }
          case _ => expr
        }
      }(arcBody)
      Lambda(Vector(weldElement, weldBuilder), weldBody)
        .toExpr(Function(Vector(weldElement.ty, weldBuilder.ty), weldBuilder.ty))
    }

    // The Lower function is the last function executed by windows
    // All we need to do is change the windower type to its aggregation type
    //def toLower: Expr = {
    //  val Lambda(Vector(weldBuilder), arcBody) = udf
    //  val weldBuilder = arcBuilder.toAggregator
    //  val weldElement = arcElement
    //  val weldBody = fix[Expr, Expr] { f => expr =>
    //    expr.ty match {
    //      case _: Windower =>
    //        expr.kind match {
    //          case e: Merge  => Merge(f(e.builder), e.value).toExpr(expr.ty.toAggregator)
    //          case e: If     => If(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty.toAggregator)
    //          case e: Select => Select(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty.toAggregator)
    //          case e: For    => For(e.iterator, e.builder, f(e.body)).toExpr(expr.ty.toAggregator)
    //          case e: Lambda => Lambda(e.params.map(_.toAppender), f(e.body)).toExpr(expr.ty.toAggregator)
    //          case e: Let    => Let(e.symbol, e.bindingTy.toAggregator, f(e.value), f(e.body)).toExpr(expr.ty.toAggregator)
    //          case _         => expr
    //        }
    //      case _ => expr
    //    }
    //  }(arcBody)
    //  Lambda(Vector(weldElement, weldBuilder), weldBody)
    //    .toExpr(Function(Vector(weldElement.ty, weldBuilder.ty), weldBuilder.ty))
    //}
  }
}
