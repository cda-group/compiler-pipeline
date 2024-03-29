package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.compiler.dataflow.ChannelStrategy._
import se.kth.cda.compiler.dataflow.IdGenerator.{DFGId, NodeId}
import se.kth.cda.compiler.dataflow.TimeKind.Ingestion
import se.kth.cda.compiler.dataflow.WindowKind.All

// Sources have N outputs
// Sinks have 1 input
//         StreamTask---Sink
//        /
//  Source--StreamTask--Sink
//        \
//         StreamTask---Sink
// Every node except for sources store ...
//   An edge to its predecessor
//   An input type
//   An index (channel to send over from the parent node)
// Every node except for sinks store ...
//   An output strategy (e.g. broadcast on all edges)
// StreamTasks store Weld template

//trait Context extends Id {
//  val trace: Option[Trace] = None
//  val scope: Option[Scope] = None
//}
trait Trace

final case class Metadata(nodes: List[Node], timestamp_extractor: Int, arc_code: String)

final case class DFG(id: String = DFGId.generate,
                     var timestamp_extractor: Int = 0,
                     var nodes: List[Node],
                     target: String = "x86-64-unknown-linux-gnu")

//case class Scope(depth: Long, parent: Option[Scope]) extends Id

final case class Node(var id: String, parallelism: Long = 1, kind: NodeKind, ord: Int = NodeId.newGlobalOrd)

sealed trait NodeKind

object NodeKind {
  final case class Source(sourceType: Type = null,
                          var format: Format = null,
                          channelStrategy: ChannelStrategy = Forward,
                          var successors: Vector[ChannelKind] = Vector.empty,
                          var kind: SourceKind = null)
      extends NodeKind
  final case class Sink(sinkType: Type = null,
                        var format: Format = null,
                        var predecessor: Node = null,
                        var kind: SinkKind = null)
      extends NodeKind
  final case class Task(var weldFunc: Expr,
                        inputType: Type,
                        var outputType: Type,
                        var predecessor: Node,
                        var successors: Vector[ChannelKind] = Vector.empty,
                        channelStrategy: ChannelStrategy = Forward,
                        var kind: TaskKind = TaskKind.Unknown,
                        var removed: Boolean = false)
      extends NodeKind
  final case class Window(channelStrategy: ChannelStrategy = Forward,
                          var predecessor: Node,
                          var successors: Vector[ChannelKind] = Vector.empty,
                          assigner: WindowAssigner,
                          function: WindowFunction,
                          time: TimeKind = Ingestion,
                          kind: WindowKind = All)
      extends NodeKind
}

final case class WindowFunction(inputType: Type,
                                outputType: Type,
                                builderType: Type,
                                init: Expr,
                                lift: Expr,
                                lower: Expr)

sealed trait ChannelKind

object ChannelKind {
  final case class Local(node: Node) extends ChannelKind
  final case class Remote(node: Node, addr: String) extends ChannelKind
}

sealed trait SourceKind

object SourceKind {
  final case class Socket(var addr: String) extends SourceKind
  final case class LocalFile(var path: String) extends SourceKind
}

sealed trait SinkKind

object SinkKind {
  final case object Debug extends SinkKind
  final case class Socket(var addr: String) extends SinkKind
  final case class LocalFile(var path: String) extends SinkKind
}

sealed trait Format

object Format {
  final case object JSON extends Format
  final case object CSV extends Format
  final case object UTF8 extends Format
}

sealed trait TaskKind

object TaskKind {
  final case object Map extends TaskKind
  final case object Filter extends TaskKind
  final case object FlatMap extends TaskKind
  final case object Join extends TaskKind
  final case object Split extends TaskKind
  final case object Unknown extends TaskKind
}

sealed trait WindowKind

object WindowKind {
  final case object Keyed extends WindowKind
  final case object All extends WindowKind
}

sealed trait WindowAssigner

object WindowAssigner {
  final case class Tumbling(length: Long) extends WindowAssigner
  final case class Sliding(length: Long, slide: Long) extends WindowAssigner
}

sealed trait TimeKind

object TimeKind {
  final case class Event(slack: Long) extends TimeKind
  final case object Processing extends TimeKind
  final case object Ingestion extends TimeKind
}

//case class DataType(value: Type, key: Option[Key] = None)

//case class Key(keyType: Type) // + some kind of access

sealed trait ChannelStrategy

object ChannelStrategy {
  final case object Shuffle extends ChannelStrategy
  final case object Forward extends ChannelStrategy
  final case object Feedback extends ChannelStrategy
  final case object Broadcast extends ChannelStrategy
}
