package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.compiler.dataflow.ChannelStrategy._
import se.kth.cda.compiler.dataflow.DFG.newId
import se.kth.cda.compiler.dataflow.SinkKind.Debug
import se.kth.cda.compiler.dataflow.SourceKind.Socket
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

object DFG {
  var idCounter = 0
  def newId: Int = {
    val id = idCounter
    idCounter += 1
    id
  }
}

trait Trace

final case class DFG(id: String = s"dfg$newId", var nodes: List[Node], target: String = "x86-64-unknown-linux-gnu")

//case class Scope(depth: Long, parent: Option[Scope]) extends Id

final case class Node(var id: String = s"node$newId", parallelism: Long = 1, kind: NodeKind)

sealed trait NodeKind

object NodeKind {
  final case class Source(sourceType: Type,
                          channelStrategy: ChannelStrategy = Forward,
                          var successors: Vector[ChannelKind] = Vector.empty,
                          kind: SourceKind = Socket("localhost", 1337))
      extends NodeKind
  final case class Sink(sinkType: Type, var predecessor: Node = null, kind: SinkKind = Debug) extends NodeKind
  final case class Task(var weldFunc: Expr,
                        inputType: Type,
                        outputType: Type,
                        var predecessor: Node,
                        var successors: Vector[ChannelKind] = Vector.empty,
                        channelStrategy: ChannelStrategy = Forward,
                        var kind: TaskKind,
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

final case class WindowFunction(inputType: Type, outputType: Type, builderType: Type, init: Expr, lift: Expr, lower: Expr)

sealed trait ChannelKind

object ChannelKind {
  final case class Local(node: Node) extends ChannelKind
  final case class Remote(node: Node, addr: String) extends ChannelKind
}

sealed trait SourceKind

object SourceKind {
  final case class Socket(host: String, port: Long) extends SourceKind
}

sealed trait SinkKind

object SinkKind {
  final case object Debug extends SinkKind
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
  final case class Keyed(kind: KeyKind) extends WindowKind
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

sealed trait KeyKind

object KeyKind {
  final case class Struct(id: String) extends KeyKind
  final case object Primitive extends KeyKind
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
