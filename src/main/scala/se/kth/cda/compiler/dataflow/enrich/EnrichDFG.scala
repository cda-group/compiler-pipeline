package se.kth.cda.compiler.dataflow.enrich

import se.kth.cda.compiler.dataflow.{DFG, Metadata, NodeKind}

object EnrichDFG {
  // Inserts metadata into DFG
  implicit class EnrichDFG(val dfg: DFG) extends AnyVal {
    def enrich(metadata: Metadata): DFG = {
      dfg.timestamp_extractor = metadata.timestamp_extractor
      val metadataNodes = metadata.nodes.map(node => node.id -> node).toMap
      dfg.nodes
        .foreach(node =>
          metadataNodes.get(node.id) match {
            case Some(metadataNode) =>
              (node.kind, metadataNode.kind) match {
                case (source: NodeKind.Source, metadataSource: NodeKind.Source) =>
                  source.format = metadataSource.format
                  source.kind = metadataSource.kind
                case (sink: NodeKind.Sink, metadataSink: NodeKind.Sink) =>
                  sink.format = metadataSink.format
                  sink.kind = metadataSink.kind
                case _ => ???
              }
            case None => ()
        })
      dfg
    }
  }
}
