package gnasher.reactive.semantics

import gnasher.kripke.{KripkeStructure, KripkeStructureBuilder}
import gnasher.pl.Valuation
import gnasher.reactive.model._


object KripkeSemantics {

  def toKripke[K <: KripkeStructure](arena: Arena, kripkeBuilder: KripkeStructureBuilder[K]): K = {

    val emptyVal = Valuation.empty(arena.vars)

    val initialStates: Set[Valuation] = {
      val initialStatesSeq =
      for (jointInit <- arena.jointInits) yield {
          jointInit.exec(emptyVal)
        }
      initialStatesSeq.toSet
    }

    var current = initialStates
    var previous = Set.empty[Valuation]
    var done = false
    while (!done) {
      var newStates = Set.empty[Valuation]
      for (v <- current -- previous) {
        val newStatesSeq = for (jointUpdate <- arena.enabled(v)) yield jointUpdate.exec(v)
        newStates = newStates ++ newStatesSeq
      }
      previous = current
      done = newStates subsetOf current
      current = current.union(newStates)
    }
    val reachableStates = current

    val relation =
      for {
        v1 <- reachableStates
        jointUpdate <- arena.enabled(v1)
        v2 = jointUpdate.exec(v1)
      } yield (v1, v2)

    // Build Kripke structure.
    reachableStates.foreach(v => kripkeBuilder.addNode(v, initialStates.contains(v)))
    relation.foreach { case (v1, v2) => kripkeBuilder.addEdge(v1, v2) }
    kripkeBuilder.build()

  }

}