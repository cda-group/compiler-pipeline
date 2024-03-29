package se.kth.cda.compiler.dataflow.optimize

import se.kth.cda.compiler.dataflow.DFG
import se.kth.cda.compiler.dataflow.NodeKind.{Sink, Task}

object OptimizeDFG {

  import se.kth.cda.compiler.dataflow.optimize.Fusion._
  import se.kth.cda.compiler.dataflow.optimize.Specialization._
  implicit class OptimizeDFG(val dfg: DFG) extends AnyVal {
    def optimize(fusion: Boolean = true): DFG = {
      //import se.kth.cda.compiler.dataflow.pretty.PrettyPrint._
      //import se.kth.cda.compiler.dataflow.deploy.Deploy._
      //println(dfg.order.pretty)
      if (fusion) {
        dfg.nodes
          .filter(_.kind match {
            case _: Sink => true
            case _ => false
          })
          .foreach(_.fuseHorizontally)

        dfg.nodes = dfg.nodes
          .filter(_.kind match {
            case task: Task => !task.removed
            case _ => true
          })
      }

      //println(encodeDFG(dfg))

      dfg.nodes
        .filter(_.kind match {
          case _: Task => true
          case _       => false
        })
        .foreach(_.specialize())

      //println(dfg.order.pretty)

      dfg
    }
  }
}
