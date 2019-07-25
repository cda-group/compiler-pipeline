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

class Node {
  val id = s"node$newId"
}

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

final case class Source(kind: SourceKind = Socket("localhost", 1337),
                        inputType: Type,
                        outputType: Type,
                        parallelism: Int = 1,
                        channelStrategy: Strategy = Forward)
    extends Node
final case class Sink(kind: SinkKind = Debug,
                      inputType: Type,
                      var predecessor: Channel = null,
                      parallelism: Int = 1,
                      channelStrategy: Strategy = Forward)
    extends Node

final case class StreamTask(kind: StreamTaskKind,
                            weldFunc: Expr,
                            inputType: Type,
                            outputType: Type,
                            predecessor: Channel,
                            parallelism: Int = 1,
                            channelStrategy: Strategy = Forward)
    extends Node

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

sealed trait StreamTaskKind

object StreamTaskKind {
  final case object Map extends StreamTaskKind
  final case object Filter extends StreamTaskKind
  final case object FlatMap extends StreamTaskKind
  final case object Join extends StreamTaskKind
  final case object Split extends StreamTaskKind
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
