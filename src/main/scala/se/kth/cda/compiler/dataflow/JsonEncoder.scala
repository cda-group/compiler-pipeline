package se.kth.cda.compiler.dataflow

import io.circe.syntax._
import io.circe.{Encoder, Json}
import se.kth.cda.arc.syntaxtree.PrettyPrint.pretty
import se.kth.cda.compiler.dataflow.ChannelKind._
import se.kth.cda.compiler.dataflow.SinkKind._
import se.kth.cda.compiler.dataflow.SourceKind._
import se.kth.cda.compiler.dataflow.StreamTaskKind._

object JsonEncoder {

  implicit val encodeDFG: Encoder[DFG] = dfg =>
    Json.obj(
      ("id", dfg.id.asJson),
      ("target", dfg.target.asJson),
      ("nodes", dfg.nodes.asJson)
  )

  implicit val encodeNode: Encoder[Node] = {
    case source: Source =>
      Json.obj(
        ("id", source.id.asJson),
        ("node_type", Json.obj(("Source", source.kind.asJson))),
        ("input_type", source.inputType.render.asJson),
        ("output_type", source.outputType.render.asJson),
        ("parallelism", source.parallelism.asJson),
        ("channel_strategy", source.channelStrategy.toString.asJson),
      )
    case streamTask: StreamTask =>
      Json.obj(
        ("id", streamTask.id.asJson),
        ("node_type", Json.obj(("StreamTask", streamTask.kind.asJson))),
        ("weld_code", pretty(streamTask.weldFunc).asJson),
        ("input_type", streamTask.inputType.render.asJson),
        ("output_type", streamTask.outputType.render.asJson),
        ("parallelism", streamTask.parallelism.asJson),
        ("channel_strategy", streamTask.channelStrategy.toString.asJson),
        ("predecessor", streamTask.predecessor.asJson),
      )
    case sink: Sink =>
      Json.obj(
        ("id", sink.id.asJson),
        ("node_type", Json.obj(("SinkType", sink.kind.asJson))),
        ("input_type", sink.inputType.render.asJson),
        ("parallelism", sink.parallelism.asJson),
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
           ("port", socket.port.asJson),
         )))
  }

  implicit val encodeStreamTaskKind: Encoder[StreamTaskKind] = {
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
    case Local => "Local".asJson
    case Remote => "Remote".asJson
  }

}
