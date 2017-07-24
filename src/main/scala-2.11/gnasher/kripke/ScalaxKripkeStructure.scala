package gnasher.kripke

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._

class ScalaxKripkeStructure(private val graph: Graph[KripkeNode, DiEdge]) extends KripkeStructure {

  override def exportAsDot(name: String): String = {
    val root = DotRootGraph(directed = true, Some(name))

    def edgeT(innerEdge: Graph[KripkeNode, DiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
      val edge = innerEdge.edge
      Some((root, DotEdgeStmt(edge.from.toOuter.name, edge.to.toOuter.name)))
    }

    def nodeT(innerNode: Graph[KripkeNode, DiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
      val attrs =
        if (innerNode.toOuter.initial) Seq(DotAttr("shape", "doublecircle"))
        else Seq.empty[DotAttr]

      Some((root, DotNodeStmt(innerNode.toOuter.name, attrs)))
    }

    graph2DotExport(graph).toDot(root, edgeT, cNodeTransformer = Some(nodeT))
  }

  override def reachableStates: Traversable[KripkeNode] = graph.nodes.toOuter

  override def initialStates: Traversable[KripkeNode] = reachableStates.filter(_.initial)
}