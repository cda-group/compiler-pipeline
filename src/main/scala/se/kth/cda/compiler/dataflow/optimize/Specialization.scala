package se.kth.cda.compiler.dataflow.optimize

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind.Lambda
import se.kth.cda.compiler.dataflow.{Node, TaskKind}
import se.kth.cda.compiler.dataflow.NodeKind.Task

object Specialization {
  import se.kth.cda.compiler.dataflow.transform.ToFlatMap._
  import se.kth.cda.compiler.dataflow.transform.ToFilter._
  import se.kth.cda.compiler.dataflow.transform.ToMap._
  import se.kth.cda.compiler.dataflow.Analyzer._

  implicit class Specialization(val node: Node) extends AnyVal {
    // TODO: Support functions with more fan-out
    def specialize(): Unit = {
      node.kind match {
        case task: Task =>
          task.weldFunc.kind match {
            case udf: Lambda =>
              val (weldFunc, kind) = (udf.selectivity, udf.fanOut, udf.mutationFree) match {
                case (1, 1, _)             => (udf.toMap, TaskKind.Map)
                case (s, 1, true) if s < 1 => (udf.toFilter, TaskKind.Filter)
                case _                     => (udf.toFlatmap, TaskKind.FlatMap)
              }
              task.weldFunc = weldFunc
              task.kind = kind
            case _ => ???
          }
        case _ => ()
      }
    }
  }
}
