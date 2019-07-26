package se.kth.cda.compiler.dataflow

import io.circe.syntax._
import io.circe.{Encoder, Json}
import se.kth.cda.arc.syntaxtree.PrettyPrint.pretty
import se.kth.cda.compiler.dataflow.ChannelKind._
import se.kth.cda.compiler.dataflow.NodeKind._
import se.kth.cda.compiler.dataflow.SinkKind._
import se.kth.cda.compiler.dataflow.SourceKind._
import se.kth.cda.compiler.dataflow.TaskKind._

object JsonEncoder {

  implicit val encodeDFG: Encoder[DFG] = dfg =>
    Json.obj(
      ("id", dfg.id.asJson),
      ("target", dfg.target.asJson),
      ("nodes", dfg.nodes.asJson)
  )

  implicit val encodeNode: Encoder[Node] = node =>
    Json.obj(
      ("id", node.id.asJson),
      ("parallelism", node.parallelism.asJson),
      ("kind", node.kind.asJson),
  )

  implicit val encodeNodeKind: Encoder[NodeKind] = {
    case source: Source =>
      Json.obj(
        ("Source",
         Json.obj(
           ("source_type", source.sourceType.render.asJson),
           ("channel_strategy", source.channelStrategy.toString.asJson),
           ("kind", source.kind.asJson),
         )))
    case task: Task =>
      Json.obj(
        ("StreamTask",
         Json.obj(
           ("weld_code", pretty(task.weldFunc).asJson),
           ("input_type", task.inputType.render.asJson),
           ("output_type", task.outputType.render.asJson),
           ("channel_strategy", task.channelStrategy.toString.asJson),
           ("predecessor", task.predecessor.asJson),
           ("kind", task.kind.asJson),
         )))
    case sink: Sink =>
      Json.obj(
        ("sink_type", sink.sinkType.render.asJson),
        ("channel_strategy", sink.channelStrategy.toString.asJson),
        ("predecessor", sink.predecessor.asJson),
      )
  }

  implicit val encodeSourceKind: Encoder[SourceKind] = {
    case socket: Socket =>
      Json.obj(
        ("Socket",
         Json.obj(
           ("host", socket.host.asJson),
           ("port", socket.port.asJson)
         )))
  }

  implicit val encodeTaskKind: Encoder[TaskKind] = {
    case Map     => "Map".asJson
    case Filter  => "Filter".asJson
    case FlatMap => "FlatMap".asJson
    case Join    => "Join".asJson
    case Split   => "Split".asJson
  }

  implicit val encodeSinkKind: Encoder[SinkKind] = {
    case Debug => "Debug".asJson
  }

  implicit val encodeChannel: Encoder[Channel] = channel => {
    Json.obj(
      ("id", channel.from.id.asJson),
      ("channel_type", channel.kind.asJson),
    )
  }

  implicit val encodeChannelKind: Encoder[ChannelKind] = {
    case Local  => "Local".asJson
    case Remote => "Remote".asJson
  }

}
