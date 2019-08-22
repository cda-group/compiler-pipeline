package se.kth.cda.compiler.dataflow.encode

import io.circe.syntax._
import io.circe.{Encoder, Json}
import se.kth.cda.arc.syntaxtree.PrettyPrint.pretty
import se.kth.cda.compiler.dataflow.ChannelKind.{Local, Remote}
import se.kth.cda.compiler.dataflow.ChannelStrategy.{Broadcast, Feedback, Forward, Shuffle}
import se.kth.cda.compiler.dataflow.IdGenerator.StructId
import se.kth.cda.compiler.dataflow.NodeKind.{Sink, Source, Task, Window}
import se.kth.cda.compiler.dataflow.TaskKind._
import se.kth.cda.compiler.dataflow.TimeKind.{Event, Ingestion, Processing}
import se.kth.cda.compiler.dataflow.WindowAssigner.{Sliding, Tumbling}
import se.kth.cda.compiler.dataflow.WindowKind.{All, Keyed}
import se.kth.cda.compiler.dataflow._
import se.kth.cda.compiler.dataflow.encode.EncodeType._

object EncodeDFG {

  implicit val encodeDFG: Encoder[DFG] = dfg =>
    Json.obj(
      ("id", dfg.id.asJson),
      ("target", dfg.target.asJson),
      ("nodes", dfg.nodes.asJson),
      ("timestamp_extractor", dfg.timestamp_extractor.asJson),
  )

  implicit val encodeNode: Encoder[Node] = node =>
    Json.obj(
      ("id", node.id.asJson),
      ("parallelism", node.parallelism.asJson),
      ("kind", node.kind.asJson(node)),
  )

  implicit def encodeNodeKind(node: Node): Encoder[NodeKind] = {
    case source: Source =>
      val outputId = source.successors(0) match {
        case Local(succ) => StructId.from(node.id, succ.id)
        case Remote(succ, _) => StructId.from(node.id, succ.id)
      }
      Json.obj(
        ("Source",
          Json.obj(
            ("source_type", source.sourceType.asJson(encodeType(outputId))),
            ("format", source.format.asJson),
            ("channel_strategy", source.channelStrategy.asJson),
            ("successors", source.successors.asJson),
            ("kind", source.kind.asJson),
          )))
    case task: Task =>
      val (a, b) = getInputId(task.predecessor, node.id)
      val inputId = StructId.from(a, b)
      val outputId = task.kind match {
        case TaskKind.Filter => inputId
        case _ => task.successors(0) match {
            case Local(succ) => StructId.from(node.id, succ.id)
            case Remote(succ, _) => StructId.from(node.id, succ.id)
          }
      }
      Json.obj(
        ("Task",
          Json.obj(
            ("weld_code", pretty(task.weldFunc).asJson),
            ("input_type", task.inputType.asJson(encodeType(inputId))),
            ("output_type", task.outputType.asJson(encodeType(outputId))),
            ("channel_strategy", task.channelStrategy.asJson),
            ("predecessor", task.predecessor.id.asJson),
            ("successors", task.successors.asJson),
            ("kind", task.kind.asJson),
          )))
    case sink: Sink =>
      val inputId = StructId.from(sink.predecessor.id, node.id)
      Json.obj(
        ("Sink",
          Json.obj(
            ("sink_type", sink.sinkType.asJson(encodeType(inputId))),
            ("format", sink.format.asJson),
            ("predecessor", sink.predecessor.id.asJson),
            ("kind", sink.kind.asJson),
          )))
    case window: Window =>
      val (a, b) = getInputId(window.predecessor, node.id)
      val inputId = StructId.from(a, b)
      val outputId = window.successors(0) match {
        case Local(succ) => StructId.from(node.id, succ.id)
        case Remote(succ, _) => StructId.from(node.id, succ.id)
      }
      Json.obj(
        ("Window",
          Json.obj(
            ("channel_strategy", window.channelStrategy.asJson),
            ("predecessor", window.predecessor.id.asJson),
            ("successors", window.successors.asJson),
            ("assigner", window.assigner.asJson),
            ("window_function", window.function.asJson(encodeWindowFunction(inputId, outputId))),
            ("time_kind", window.time.asJson),
            ("window_kind", window.kind.asJson),
          )))
  }

  implicit val encodeSourceKind: Encoder[SourceKind] = {
    case socket: SourceKind.Socket =>
      Json.obj(
        ("Socket",
         Json.obj(
           ("addr", socket.addr.asJson),
         ),
        ),
      )
    case localfile: SourceKind.LocalFile =>
      Json.obj(
        ("LocalFile",
         Json.obj(
           ("path", localfile.path.asJson),
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
    case SinkKind.Debug => "Debug".asJson
    case socket: SinkKind.Socket =>
      Json.obj(
        ("Socket",
         Json.obj(
           ("addr", socket.addr.asJson),
         ),
        ))
    case localfile: SinkKind.LocalFile =>
      Json.obj(
        ("LocalFile",
         Json.obj(
           ("path", localfile.path.asJson),
         ),
        )
      )
  }

  implicit val encodeFormat: Encoder[Format] = {
    case Format.CSV  => "CSV".asJson
    case Format.JSON => "JSON".asJson
    case Format.UTF8 => "UTF8".asJson
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

  implicit def encodeWindowFunction(inputId: String, outputId: String): Encoder[WindowFunction] =
    function =>
      Json.obj(
        ("input_type", function.inputType.asJson(encodeType(inputId))),
        ("output_type", function.outputType.asJson(encodeType(outputId))),
        ("builder_type", function.builderType.asJson(encodeType())),
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
    case Keyed => "Keyed".asJson
    case All   => "All".asJson
  }

  // Walks backwards and finds the first output coming from a non-filter
  private def getInputId(node: Node, succ_id: String): (String, String) = {
    node.kind match {
      case Task(_, _, _, pred, _, _, Filter, _) => getInputId(pred, node.id)
      case _: Source | _: Task | _: Window | _: Sink => (node.id, succ_id)
    }
  }

}
