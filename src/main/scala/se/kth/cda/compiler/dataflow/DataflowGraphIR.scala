package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.{ AST, Type => ArcType }

object DataflowGraphIR {
  import java.util.UUID;
  def nodeId: String = s"node-${UUID.randomUUID()}";
  def sourceId: String = s"src-${UUID.randomUUID()}";
  def sinkId: String = s"sink-${UUID.randomUUID()}";
  def edgeId: String = s"edge-${UUID.randomUUID()}";
}

trait DFGraph
trait DGEdge
trait DGNode
trait Trace

case class MetaInfo(id: String, trace: Option[Trace] = None, scope: Option[Scope] = None)

case class Graph(sources: List[Source]) extends DFGraph

case class Scope(id: String, depth: Int, parent: Option[Scope])

case class DefaultNode(info: MetaInfo, template: NodeTemplate, var inputs: List[DGEdge] = Nil, outputs: List[DGEdge] = Nil) extends DGNode {
  override def toString(): String = s"DefaultNode(info=$info, template=$template, inputs=<skipped>, outputs=$outputs)";
}

case class Source(info: MetaInfo, out: List[DGEdge] = Nil) extends DGNode
object Source {
  def empty: Source = Source(
    info = MetaInfo(id = DataflowGraphIR.sourceId))

  def withOut(edges: DGEdge*): Source = Source(
    info = MetaInfo(id = DataflowGraphIR.sourceId),
    out = edges.toList)
}

case class Sink(info: MetaInfo, var in: List[DGEdge] = Nil) extends DGNode {
  override def toString(): String = s"Sink(info=$info, in=<skipped>)"
}
object Sink {
  def empty: Sink = Sink(
    info = MetaInfo(id = DataflowGraphIR.sinkId))
}

case class Edge(info: MetaInfo, edgeType: EdgeType, dataType: Type, var from: DGNode, to: DGNode) extends DGEdge {
  override def toString(): String = s"Edge(info=$info, edgeType=$edgeType, from=<skipped>, to=$to)"
}
object Edge {
  def forward(dataType: Type, from: DGNode, to: DGNode): Edge = Edge(
    info = MetaInfo(id = DataflowGraphIR.edgeId),
    edgeType = EdgeType.Forward,
    dataType = dataType,
    from = from,
    to = to);
}

sealed trait NodeTemplate;
object NodeTemplate {
  import se.kth.cda.arc.syntaxtree.PrettyPrint

  case class Map(weldBody: AST.Program) extends NodeTemplate {
    override def toString(): String = s"""Map(weldBody = "
${PrettyPrint.pretty(weldBody)}
"
)""";
  }
  object Map {
    def withBody(weldBody: AST.Program): DefaultNode = DefaultNode(
      info = MetaInfo(id = DataflowGraphIR.nodeId),
      template = Map(weldBody));
  }
}

case class Type(dataType: ArcType, key: Option[Key])
object Type {
  def fromArc(ty: ArcType): Type = Type(dataType = ty, key = None);
}
case class Key(keyType: ArcType) // + some kind of access

sealed trait EdgeType;
object EdgeType {
  case object Shuffle extends EdgeType;
  case object Forward extends EdgeType;
  case object Feedback extends EdgeType; // back edge
  case object Broadcast extends EdgeType;
}
