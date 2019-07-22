package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.Type.Builder.{StreamAppender, Windower}
import se.kth.cda.arc.syntaxtree.Type.{Stream, Struct}
import se.kth.cda.arc.syntaxtree.{Type => ArcType, _}
import se.kth.cda.compiler.dataflow.Analyzer._
import se.kth.cda.compiler.dataflow.OperatorTemplate.{Flatmapper, Mapper}
import se.kth.cda.compiler.dataflow.Transformer._

import scala.util.Try

object Converter {

  // Arc operators are flatmaps from 1→N channels
  // If the Weld body has selectivity = 1, it can be optimized into a map
  // If the Weld body is commutative (pure unary function), it can be data-parallelized
  def transform(input: AST.ArcStatements): DFG = {
    val (arcParams, body) = extractArcParams(input.exprs.head)
    val dfg = transformExpr(arcParams, body)
    dfg
  }

  def extractArcParams(expr: AST.Expr): (Map[String, ArcParam], AST.Expr) = {
    import AST.ExprKind._

    expr.kind match {
      case Lambda(params, body) => {
        val arcParams = params
          .flatMap(ArcParam.from(_).toOption)
          .map(a => a.symbol.name -> a)
          .toMap
        (arcParams, body)
      }
      case _ => ???
    }
  }

  private def transformExpr(arcParams: Map[String, ArcParam], arcExpr: AST.Expr): DFG = {
    import AST.ExprKind._
    import AST.IterKind

    arcExpr.kind match {
      // for(stream, streamappender, body)
      case arcFor: For =>
        arcFor.iterator.kind match {
          case IterKind.KeyByIter => ???
          case IterKind.NextIter | IterKind.UnknownIter => // TODO don't have unknown iter here!!!
            arcFor.builder.ty match {
              case _: StreamAppender =>
                arcFor.body.kind match {
                  // |streamappender, index, event| body
                  case arcLambda: Lambda =>
                    // TODO: Currently we assume the builder argument is an identifier
                    val builderInit = arcFor.builder.kind.asInstanceOf[Ident].symbol
                    val template = selectivity(arcLambda) match {
                      // case s if s == 1 => Mapper(arcLambda.intoWeldMap()) TODO: Implement mapper transformation
                      // case s if s <= 1 => Filter(arcLambda.intoWeldFilter()) TODO: Implement filter transformation
                      case _ => Flatmapper(arcFor.body.toWeldFlatmap)
                    }
                    // TODO: This will not work for multiple operators
                    val output = arcParams(builderInit.name).asInstanceOf[ArcParam.SinkParam]
                    val input = arcFor.iterator.data.kind match {
                      case Ident(symbol) => arcParams(symbol.name).asInstanceOf[ArcParam.SourceParam]
                      case _ => ???
                    }
                    val source = Source()
                    val inputEdge = Edge.forward(DataType(input.ty), from = source)
                    val operator = Operator(template, inputs = List(inputEdge))
                    inputEdge.to = operator
                    val outputEdge = Edge.forward(DataType(output.ty), from = operator)
                    operator.outputs = List(outputEdge)
                    val sink = Sink(inputs = List(outputEdge))
                    outputEdge.to = sink
                    Graph(List(source))
                  case _ => ???
                }
              case _: Struct => ???
              case _: Windower => ??? // TODO
              case _ => ???
            }
          case _ => ???
        }
      case _ => ???
    }
  }

}

sealed trait ArcParam {
  def symbol: AST.Symbol
}

object ArcParam {

  case class SourceParam(symbol: AST.Symbol, ty: ArcType) extends ArcParam

  case class SinkParam(symbol: AST.Symbol, ty: ArcType) extends ArcParam

  def from(param: AST.Parameter): Try[ArcParam] = Try {
    param.ty match {
      case stream: Stream => SourceParam(param.symbol, stream.elemTy)
      case streamapp: StreamAppender => SinkParam(param.symbol, streamapp.elemTy)
      case _ => ???
    }
  }
}
