package se.kth.cda.compiler.dataflow.pretty

import se.kth.cda.compiler.dataflow.{DFG, Node, NodeKind}

object PrettyPrint {

  implicit class PrettyPrintDFG(val dfg: DFG) extends AnyVal {
    def pretty: String =
      s""""
         |# DFG
         |nodes: ${dfg.nodes.map(_.pretty).mkString("")}
         |----
      """.stripMargin
  }

  implicit class PrettyPrintNode(val node: Node) extends AnyVal {
    def pretty: String = node.kind match {
      case NodeKind.Source(sourceType, format, channelStrategy, successors, kind) =>
        s"""
           |:  Source: {
           |:    id: ${node.id},
           |:    sourceType: ${sourceType.render},
           |:    format: $format,
           |:    kind: $kind,
           |:  }
         """.stripMargin
      case NodeKind.Sink(sinkType, format, predecessor, kind) =>
        s"""
           |:  Sink: {
           |:    id: ${node.id},
           |:    sinkType: ${sinkType.render},
           |:    format: $format,
           |:    kind: $kind,
           |:  }
         """.stripMargin
      case NodeKind.Task(weldFunc, inputType, outputType, predecessor, successors, channelStrategy, kind, removed) =>
        s"""
           |:  Task: {
           |:    id: ${node.id},
           |:    inputType: ${inputType.render},
           |:    outputType: ${outputType.render},
           |:    kind: $kind,
           |:  }
         """.stripMargin
      case NodeKind.Window(channelStrategy, predecessor, successors, assigner, function, time, kind) =>
        s"""
           |:  Window: {
           |:    id: ${node.id},
           |:    time: $time,
           |:    kind: $kind,
           |:    inputType: ${function.inputType.render},
           |:    outputType: ${function.outputType.render},
           |:  }
         """.stripMargin
    }
  }
}
