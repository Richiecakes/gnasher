package gnasher.kripke

import gnasher.pl.Valuation

trait KripkeStructure {
  def exportAsDot(name: String): String
  def reachableStates: Traversable[KripkeNode]
  def initialStates: Traversable[KripkeNode]
}

case class KripkeNode(name: String, valuation: Valuation, initial: Boolean)

trait KripkeStructureBuilder[K <: KripkeStructure] {
  def addNode(v: Valuation, initial: Boolean)
  def addEdge(v1: Valuation, v2: Valuation)
  def build(): K
}