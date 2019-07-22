package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.{AST, Type}
import se.kth.cda.compiler.dataflow.OperatorTemplate.Mapper

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

case class Operator(template: OperatorTemplate,
                    inputs: List[DFGEdge] = Nil,
                    var outputs: List[DFGEdge] = Nil,
                    metadata: Metadata = Metadata(DFG.newNodeId))
  extends DFGNode

case class Source(var outputs: List[DFGEdge] = Nil, info: Metadata = Metadata(DFG.newSourceId)) extends DFGNode

case class Sink(inputs: List[DFGEdge] = Nil, info: Metadata = Metadata(DFG.newSinkId)) extends DFGNode

case class Edge(info: Metadata, kind: EdgeKind, dataType: DataType, from: DFGNode, var to: DFGNode) extends DFGEdge

object Edge {

  def forward(dataType: DataType, from: DFGNode = null, to: DFGNode = null): Edge =
    Edge(Metadata(DFG.newEdgeId), EdgeKind.Forward, dataType, from, to)
}

sealed trait OperatorTemplate

object OperatorTemplate {

  case class Mapper(weldBody: AST.Expr) extends OperatorTemplate

  case class Filterer(weldBody: AST.Expr) extends OperatorTemplate

  case class Flatmapper(weldBody: AST.Expr) extends OperatorTemplate

}

case class DataType(dataType: Type, key: Option[Key] = None)

case class Key(keyType: Type) // + some kind of access

sealed trait EdgeKind

object EdgeKind {

  case object Shuffle extends EdgeKind
  case object Forward extends EdgeKind
  case object Feedback extends EdgeKind // back edge
  case object Broadcast extends EdgeKind

}
