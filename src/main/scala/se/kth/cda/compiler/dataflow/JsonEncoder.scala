package se.kth.cda.compiler.dataflow

import io.circe.syntax._
import io.circe.{Encoder, Json}
import se.kth.cda.arc.syntaxtree.PrettyPrint.pretty
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.compiler.dataflow.ChannelKind._
import se.kth.cda.compiler.dataflow.NodeKind._
import se.kth.cda.compiler.dataflow.SinkKind._
import se.kth.cda.compiler.dataflow.SourceKind._
import se.kth.cda.compiler.dataflow.ChannelStrategy._
import se.kth.cda.compiler.dataflow.KeyKind._
import se.kth.cda.compiler.dataflow.TaskKind._
import se.kth.cda.compiler.dataflow.TimeKind._
import se.kth.cda.compiler.dataflow.WindowAssigner._
import se.kth.cda.compiler.dataflow.WindowKind._

object JsonEncoder {

  implicit val encodeDFG: Encoder[DFG] = dfg =>
    Json.obj(
      ("id", dfg.id.asJson),
      ("target", dfg.target.asJson),
      ("nodes", dfg.nodes.asJson),
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
           ("channel_strategy", source.channelStrategy.asJson),
           ("successors", source.successors.asJson),
           ("kind", source.kind.asJson),
         )))
    case task: Task =>
      Json.obj(
        ("StreamTask",
         Json.obj(
           ("weld_code", pretty(task.weldFunc).asJson),
           ("input_type", task.inputType.render.asJson),
           ("output_type", task.outputType.render.asJson),
           ("channel_strategy", task.channelStrategy.asJson),
           ("predecessor", task.predecessor.id.asJson),
           ("successors", task.successors.asJson),
           ("kind", task.kind.asJson),
         )))
    case sink: Sink =>
      Json.obj(
        ("Sink",
         Json.obj(
           ("sink_type", sink.sinkType.render.asJson),
           ("predecessor", sink.predecessor.id.asJson),
         )))
    case window: Window =>
      Json.obj(
        ("Window",
         Json.obj(
           ("channel_strategy", window.channelStrategy.asJson),
           ("predecessor", window.predecessor.id.asJson),
           ("successors", window.successors.asJson),
           ("assigner", window.assigner.asJson),
           ("window_function", window.function.asJson),
           ("time_kind", window.time.asJson),
           ("window_kind", window.kind.asJson),
         )))
  }

  implicit val encodeSourceKind: Encoder[SourceKind] = {
    case socket: Socket =>
      Json.obj(
        ("Socket",
         Json.obj(
           ("host", socket.host.asJson),
           ("port", socket.port.asJson),
         ),
        ),
      )
  }

  implicit val encodeTaskKind: Encoder[TaskKind] = {
    case Map     => "Map".asJson
    case Filter  => "Filter".asJson
    case FlatMap => "FlatMap".asJson
    case Join    => "Join".asJson
    case Split   => "Split".asJson
    case Unknown => "Unknown".asJson
  }

  implicit val encodeSinkKind: Encoder[SinkKind] = {
    case Debug => "Debug".asJson
  }

  implicit val encodeChannelKind: Encoder[ChannelKind] = {
    case local: Local => Json.obj(("Local", Json.obj(("id", local.node.id.asJson))))
    case remote: Remote =>
      Json.obj(
        ("Remote", Json.obj(("id", remote.node.id.asJson), ("addr", remote.addr.asJson))),
      )
  }

  implicit val encodeChannelStrategy: Encoder[ChannelStrategy] = {
    case Shuffle   => "Shuffle".asJson
    case Forward   => "Forward".asJson
    case Feedback  => "Feedback".asJson
    case Broadcast => "Broadcast".asJson
  }

  implicit val encodeWindowFunction: Encoder[WindowFunction] = function =>
    Json.obj(
      ("input_type", function.inputType.render.asJson),
      ("output_type", function.outputType.render.asJson),
      ("builder_type", function.builderType.asJson),
      ("builder", pretty(function.init).asJson),
      ("udf", pretty(function.lift).asJson),
      ("materialiser", pretty(function.lower).asJson),
  )

  implicit val encodeWindowAssigner: Encoder[WindowAssigner] = {
    case tumbling: Tumbling =>
      Json.obj(("Tumbling", Json.obj(("length", tumbling.length.asJson))))
    case sliding: Sliding =>
      Json.obj(("Sliding", Json.obj(("length", sliding.length.asJson), ("slide", sliding.slide.asJson))))
  }

  implicit val encodeTimeKind: Encoder[TimeKind] = {
    case event: Event => Json.obj(("Event", Json.obj(("slack", event.slack.asJson))))
    case Processing   => "Processing".asJson
    case Ingestion    => "Ingestion".asJson
  }

  implicit val encodeWindowKind: Encoder[WindowKind] = {
    case keyed: Keyed => Json.obj(("Keyed", Json.obj(("kind", keyed.kind.asJson))))
    case All          => "All".asJson
  }

  implicit val encodeKeyKind: Encoder[KeyKind] = {
    case struct: Struct => Json.obj(("Struct", Json.obj(("index", struct.id.asJson))))
    case Primitive      => "Primitive".asJson
  }

  implicit val encodeType: Encoder[Type] = {
    case _: Appender    => "Appender".asJson
    case _: Merger      => "Merger".asJson
    case _: VecMerger   => "VecMerger".asJson
    case _: DictMerger  => "DictMerger".asJson
    case _: GroupMerger => "GroupMerger".asJson
    case _              => ???
  }

}
