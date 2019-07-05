package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.{AST, Type => ArcType}

object DFG {
  import java.util.UUID

  def newNodeId: String = s"node-${UUID.randomUUID()}"

  def newSourceId: String = s"source-${UUID.randomUUID()}"

  def newSinkId: String = s"sink-${UUID.randomUUID()}"

  def newEdgeId: String = s"edge-${UUID.randomUUID()}"
}

trait DFG
trait DFGEdge
trait DFGNode
trait Trace

case class Metadata(id: String, trace: Option[Trace] = None, scope: Option[Scope] = None)

case class Graph(sources: List[Source]) extends DFG

case class Scope(id: String, depth: Int, parent: Option[Scope])

case class Operator(
    metadata: Metadata,
    template: OperatorTemplate,
    var inputs: List[DFGEdge] = Nil,
    outputs: List[DFGEdge] = Nil)
    extends DFGNode

case class Source(info: Metadata, out: List[DFGEdge] = Nil) extends DFGNode

object Source {
  def empty: Source = Source(Metadata(DFG.newSourceId))

  def withOut(edges: DFGEdge*): Source = Source(Metadata(DFG.newSourceId), out = edges.toList)
}

case class Sink(info: Metadata, var in: List[DFGEdge] = Nil) extends DFGNode

object Sink {
  def empty: Sink = Sink(Metadata(DFG.newSinkId))
}

case class Edge(info: Metadata, edgeType: EdgeType, dataType: Type, var from: DFGNode, to: DFGNode) extends DFGEdge

object Edge {
  def forward(dataType: Type, from: DFGNode, to: DFGNode): Edge =
    Edge(Metadata(DFG.newEdgeId), EdgeType.Forward, dataType, from, to)
}

sealed trait OperatorTemplate

object OperatorTemplate {
  case class Mapper(weldBody: AST.Program) extends OperatorTemplate

  object Mapper {

    def withBody(weldBody: AST.Program): Operator =
      Operator(Metadata(DFG.newNodeId), Mapper(weldBody))
  }
}

case class Type(dataType: ArcType, key: Option[Key])

object Type {
  def fromArc(ty: ArcType): Type = Type(ty, None)
}

case class Key(keyType: ArcType) // + some kind of access

sealed trait EdgeType

object EdgeType {
  case object Shuffle extends EdgeType
  case object Forward extends EdgeType
  case object Feedback extends EdgeType // back edge
  case object Broadcast extends EdgeType
}
