package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.{ Type => ArcType }

object DataflowGraphIR {

}

trait DFGraph

trait DGNode

case class Node(template: NodeTemplate, inChannels: List[Channel], outChannels: List[Channel]) extends DGNode

case class Source(outChannels: List[Channel]) extends DGNode
case class Sink(inChannel: List[Channel]) extends DGNode

case class Channel(channelType: ChannelType, dataType: Type, from: Node, to: Node) extends DGNode

case class Scope(id: String, depth: Int, content: DFGraph) extends DGNode

sealed trait NodeTemplate;

case class Type(dataType: ArcType, key: Option[Key])
case class Key(keyType: ArcType) // + some kind of access

sealed trait ChannelType;
object ChannelType {
  case object Shuffle extends ChannelType;
  case object Forward extends ChannelType;
  case object Feedback extends ChannelType; // back edge
  case object Broadcast extends ChannelType;
}
