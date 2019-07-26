package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.AST.IterKind._
import se.kth.cda.arc.syntaxtree.AST._
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.compiler.dataflow.Analyzer._
import se.kth.cda.compiler.dataflow.NodeKind._
import se.kth.cda.compiler.dataflow.TaskKind._
import se.kth.cda.compiler.dataflow._

object ToDFG {

  import se.kth.cda.compiler.dataflow.transform.ToFlatMap._
  import se.kth.cda.compiler.dataflow.transform.ToMap._
  import se.kth.cda.compiler.dataflow.transform.ToFilter._

  implicit class ToDFG(val expr: Expr) extends AnyVal {

    // Arc streamTasks are flatmaps from 1→N channels
    // If the Weld body has selectivity = 1, it can be optimized into a map
    // If the Weld body is commutative (pure unary function), it can be data-parallelized
    def toDFG: DFG = {
      // Extract input sources/sinks
      val (nodes, body) = expr.kind match {
        case lambda: Lambda =>
          (lambda.params.map {
            case Parameter(symbol, StreamAppender(elemTy, _)) => symbol.name -> Node(kind = Sink(sinkType = elemTy))
            case Parameter(symbol, Stream(elemTy))            => symbol.name -> Node(kind = Source(sourceType = elemTy))
            case _                                            => ???
          }.toMap, lambda.body)
        case _ => ???
      }
      DFG(nodes = transformRec(body, nodes))
    }

  }

  def transformRec(expr: Expr, nodes: Map[String, Node]): List[Node] = {
    // TODO: For now expect the Arc code to be a nesting of Let-expressions
    // TODO: which ends with a for-expression
    expr.kind match {
      // let source = result(for(source, sink, ...)); (Add a new streamTask)
      case Let(Symbol(taskName, _, _), _, Expr(Result(Expr(arcFor: For, _, _, _)), _, _, _), body) =>
        transformRec(body, nodes + (taskName -> newStreamTask(arcFor, nodes)))
      // for(source, external_sink, ...) (Add a new streamTask and link it to an external sink)
      case arcFor @ For(_, Expr(Ident(Symbol(sinkName, _, _)), _, _, _), _) =>
        nodes(sinkName).kind match {
          case sink: Sink =>
            val task = newStreamTask(arcFor, nodes)
            sink.predecessor = Channel(from = task)
            task +: nodes.values.toList // TODO: Allow multiple sinks
          case _ => ???
        }
      case _ => ???
    }
  }

  def newStreamTask(arcFor: For, nodes: Map[String, Node]): Node = {
    arcFor match {
      // TODO: Only one output stream for now
      case For(iter, Expr(_, StreamAppender(outputType, _), _, _), Expr(udf: Lambda, _, _, _)) =>
        // Convert the Arc UDF to a Weld UDF
        // TODO: Support functions with more fan-in
        val (weldFunc, kind) = (selectivity(udf), fan_out(udf)) match {
          case (1, 1)          => (udf.toMap, Map)
          //case (s, 1) if s < 1 => (udf.toFilter, Filter)
          case _               => (udf.toFlatmap, FlatMap)
        }
        // Next, create the streamTask
        iter match {
          // Non-keyed stream
          case Iter(NextIter | UnknownIter, source, _, _, _, _, _, _) =>
            val inputType = source.ty match {
              case Stream(elemTy) => elemTy
              case _              => ???
            }
            val (from, index) = source.kind match {
              // for(source, sink, ...)
              case Ident(Symbol(sourceName, _, _)) => (nodes(sourceName), 0)
              // for(source.$0, sink, ...)
              case Projection(Expr(Ident(Symbol(sourceName, _, _)), _, _, _), i) => (nodes(sourceName), i)
              case _                                                             => ???
            }
            Node(
              kind = Task(kind = kind,
                          weldFunc = weldFunc,
                          inputType = inputType,
                          outputType = outputType,
                          predecessor = Channel(from = from, index = index)))
          // Keyed stream
          case Iter(KeyByIter, source, _, _, _, _, _, keyFunc) => ???
          case _                                               => ???
        }
      case _ => ???
    }
  }

}
