package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.compiler.dataflow.ChannelKind.Local
import se.kth.cda.compiler.dataflow.DFG.newId
import se.kth.cda.compiler.dataflow.SinkKind.Debug
import se.kth.cda.compiler.dataflow.SourceKind.Socket
import se.kth.cda.compiler.dataflow.Strategy._

object DFG {
  var idCounter = 0
  def newId: Int = {
    val id = idCounter
    idCounter += 1
    id
  }
}

//trait Context extends Id {
//  val trace: Option[Trace] = None
//  val scope: Option[Scope] = None
//}

trait Trace

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

// Until we finalize the syntax, the DFG is a linear pipeline
final case class DFG(id: String = s"dfg$newId", nodes: List[Node], target: String = "x86-64-unknown-linux-gnu")

//case class Scope(depth: Int, parent: Option[Scope]) extends Id

final case class Node(id: String = s"node$newId", parallelism: Int = 1, kind: NodeKind)

sealed trait NodeKind

object NodeKind {
  final case class Source(sourceType: Type,
                          channelStrategy: Strategy = Forward,
                          kind: SourceKind = Socket("localhost", 1337))
      extends NodeKind
  final case class Sink(sinkType: Type,
                        var predecessor: Channel = null,
                        channelStrategy: Strategy = Forward,
                        kind: SinkKind = Debug)
      extends NodeKind
  final case class Task(weldFunc: Expr,
                        inputType: Type,
                        outputType: Type,
                        predecessor: Channel,
                        channelStrategy: Strategy = Forward,
                        kind: TaskKind)
      extends NodeKind
}

final case class Channel(kind: ChannelKind = Local, from: Node, index: Int = 0)

sealed trait ChannelKind

object ChannelKind {
  final case object Local extends ChannelKind
  final case object Remote extends ChannelKind
}

sealed trait SourceKind

object SourceKind {
  final case class Socket(host: String, port: Int) extends SourceKind
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
}

//case class DataType(value: Type, key: Option[Key] = None)

//case class Key(keyType: Type) // + some kind of access

sealed trait Strategy

object Strategy {
  final case object Shuffle extends Strategy
  final case object Forward extends Strategy
  final case object Feedback extends Strategy // back edge
  final case object Broadcast extends Strategy
}

sealed trait NodeType

object NodeType {
  final case class Source(host: String, port: Int) extends NodeType
  final case class Sink(host: String) extends NodeType
  final case object StreamTask extends NodeType
}
