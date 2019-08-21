package se.kth.cda.compiler.dataflow.transform

import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.AST.IterKind._
import se.kth.cda.arc.syntaxtree.AST._
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.compiler.dataflow.ChannelKind.Local
import se.kth.cda.compiler.dataflow._
import se.kth.cda.compiler.dataflow.NodeKind._
import se.kth.cda.compiler.dataflow.IdGenerator._

object ToDFG {

  import se.kth.cda.compiler.dataflow.transform.ToWindow._
  import se.kth.cda.compiler.dataflow.transform.Utils._

  implicit class ToDFG(val expr: Expr) extends AnyVal {

    // Arc streamTasks are flatmaps from 1→N channels
    // If the Weld body has selectivity = 1, it can be optimized into a map
    // If the Weld body is commutative (pure unary function), it can be data-parallelized
    def toDFG: DFG = {
      // Extract input sources/sinks
      val (nodes, body) = expr.kind match {
        case lambda: Lambda =>
          (lambda.params.map {
            case Parameter(symbol, StreamAppender(elemTy, _)) =>
              symbol.name -> Node(id = SinkId.newId, kind = Sink(sinkType = elemTy))
            case Parameter(symbol, Stream(elemTy)) =>
              symbol.name -> Node(id = SourceId.newId, kind = Source(sourceType = elemTy))
            case _ => ???
          }.toMap, lambda.body)
        case _ => ???
      }
      DFG(nodes = transform(body, nodes))
    }

  }

  def transform(expr: Expr, nodes: Map[String, Node]): List[Node] = {
    // TODO: For now expect the Arc code to be a sequence of Let-expressions, which ends with a for-expression
    expr.kind match {
      // let node = result(for(source, sink, ...)); (Add a new node)
      case Let(Symbol(taskName, _, _), _, Expr(Result(Expr(arcFor: For, _, _, _)), _, _, _), body) =>
        transform(body, nodes + (taskName -> transformFor(arcFor, nodes)))
      // for(source, external_sink, ...) (Add a new node and link it to an external sink)
      case arcFor @ For(_, Expr(Ident(Symbol(sinkName, _, _)), _, _, _), _) =>
        val node = nodes(sinkName)
        node.kind match {
          case sink: Sink =>
            val operator = transformFor(arcFor, nodes)
            sink.predecessor = operator
            sink.predecessor.kind match {
              case task: Task => task.successors = task.successors :+ Local(node = node)
            }
            operator +: nodes.values.toList // TODO: Allow multiple sinks
          case _ => ???
        }
      case _ => ???
    }
  }

  def transformFor(arcFor: For, nodes: Map[String, Node]): Node =
    arcFor match {
      // TODO: Only one output stream for now
      case For(iter, sink, func) =>
        val (inputType, precedessor, _) = transformSource(iter, nodes)
        val nodeKind = transformSink(sink, func, inputType, precedessor)
        // Add node as successor to predecessor
        val newNode = Node(id = TaskId.newId, kind = nodeKind)
        precedessor.kind match {
          case source: Source => source.successors = source.successors :+ Local(node = newNode)
          case task: Task     => task.successors = task.successors :+ Local(node = newNode)
          case window: Window => window.successors = window.successors :+ Local(node = newNode)
          case _              => ???
        }
        newNode
      //val (weldFunc, kind) =
      case _ => ???
    }

  private def transformSource(iter: Iter, nodes: Map[String, Node]): (Type, Node, Int) = {
    iter match {
      // Non-keyed stream
      case Iter(NextIter, source, _, _, _, _, _, _) =>
        val Stream(inputType) = source.ty
        val (from, index) = source.kind match {
          // for(source, sink, ...)
          case Ident(Symbol(sourceName, _, _)) => (nodes(sourceName), 0)
          // for(source.$0, sink, ...)
          case Projection(Expr(Ident(Symbol(sourceName, _, _)), _, _, _), i) => (nodes(sourceName), i)
          case _ => ???
        }
        (inputType, from, index)
      //case Iter(KeyByIter, source, _, _, _, _, _, keyFunc) =>
      case _ => ???
    }
  }

  private def transformSink(sink: Expr, func: Expr, inputType: Type, precedessor: Node): NodeKind =
    sink match {
      case Expr(_, StreamAppender(outputType, _), _, _) =>
        Task(weldFunc = func,
             inputType = inputType,
             outputType = outputType,
             predecessor = precedessor,
             successors = Vector.empty)
      case Expr(NewBuilder(Windower(_, aggrTy, _, aggrResultTy, _), Vector(a1, a2, a3)), _, _, _) =>
        (a1.kind, a2.kind, a3.kind, func.kind) match {
          case (assigner: Lambda, _: Lambda, lower: Lambda, lift: Lambda) =>
            Window(
              assigner = assigner.toAssigner,
              predecessor = precedessor,
              successors = Vector.empty,
              function = WindowFunction(
                inputType = inputType,
                outputType = lower.body.ty,
                builderType = aggrTy,
                init = aggrTy.toInstance.toFunc,
                lift.toLift,
                lower = a3
              )
            )
          case _ => ???
        }
    }

}
