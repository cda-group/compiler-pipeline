package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.{Type => ArcType}

object DataflowGraphIR {

}

trait DFGraph
trait DGEdge
trait DGNode
trait Trace 

case class MetaInfo(id: String, trace: Option[Trace], scope: Option[Scope])

case class Graph(sources: List[Source])

case class Scope(id: String, depth: Int, parent: Option[Scope])

case class DefaultNode(info: MetaInfo, template: NodeTemplate, inputs: List[DGEdge], outputs: List[DGEdge]) extends DGNode
case class Source(info: MetaInfo, out: List[DGEdge]) extends DGNode
case class Sink(info: MetaInfo, in: List[DGEdge]) extends DGNode

case class Edge(info: MetaInfo, edgeType: EdgeType, dataType: Type, from: DGNode, to: DGNode) extends DGEdge

sealed trait NodeTemplate;

case class Type(dataType: ArcType, key: Option[Key])
case class Key(keyType: ArcType) // + some kind of access

sealed trait EdgeType;
object EdgeType {
  case object Shuffle extends EdgeType;
  case object Forward extends EdgeType;
  case object Feedback extends EdgeType; // back edge
  case object Broadcast extends EdgeType;
}
