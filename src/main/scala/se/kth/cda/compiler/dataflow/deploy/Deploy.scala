package se.kth.cda.compiler.dataflow.deploy

import se.kth.cda.compiler.dataflow.{DFG, NodeKind}

object Deploy {
  implicit class Deploy(val dfg: DFG) extends AnyVal {
    def order: DFG = {
      dfg.nodes = dfg.nodes.sortBy(_.ord)
      dfg.nodes = dfg.nodes.sortBy(node =>
        node.kind match {
          case _: NodeKind.Source => 0
          case _: NodeKind.Task   => 1
          case _: NodeKind.Window => 1
          case _: NodeKind.Sink   => 2
      })
      dfg
    }
  }
}
