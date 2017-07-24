package gnasher.kripke

import gnasher.pl.Valuation

import scala.collection.mutable
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

class ScalaxKripkeStructureBuilder extends KripkeStructureBuilder[ScalaxKripkeStructure] {

  private val nodes: mutable.Map[Valuation, KripkeNode] = mutable.Map.empty[Valuation, KripkeNode]
  private val edges: mutable.Set[DiEdge[KripkeNode]] = mutable.Set.empty[DiEdge[KripkeNode]]

  private def mkName(v: Valuation): String = v.label

  override def addNode(v: Valuation, initial: Boolean): Unit = {
    val node = KripkeNode(mkName(v), v, initial)
    nodes.put(v, node)
  }

  override def addEdge(v1: Valuation, v2: Valuation): Unit = {
    val n1 = nodes.get(v1)
    val n2 = nodes.get(v2)
    if (n1.isEmpty || n2.isEmpty)
      throw new IllegalArgumentException("Cannot create edges between nodes not yet added.")
    val edge = DiEdge(n1.get, n2.get)
    edges += edge
  }

  override def build(): ScalaxKripkeStructure = new ScalaxKripkeStructure(Graph.from(nodes.values, edges))
}