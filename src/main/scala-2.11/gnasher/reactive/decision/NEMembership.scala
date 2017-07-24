package gnasher.reactive.decision

import gnasher.kripke.ScalaxKripkeStructureBuilder
import gnasher.reactive.MutexExample
import gnasher.reactive.model.{Arena, Goals, Module, Modules}
import gnasher.reactive.semantics.KripkeSemantics

object NEMembership {

  def isNE(arena: Arena, profile: Arena, goals: Goals): Boolean = {
    val modules = arena.modules.modules.toArray
    val strategies = profile.modules.modules.toArray

    val numModules = modules.length
    val numStrategies = strategies.length

    require(numModules == numStrategies, "Must be same size.")

    for (i <- modules.indices) {

      // Player i
      val module = modules(i)
      val playerName = module.name
      val goal = goals.goals(playerName)

      val kb = new ScalaxKripkeStructureBuilder
      val profileKripke = KripkeSemantics.toKripke(profile, kb)
      val oLTLRealizability = new OLTLRealizability(profile, profileKripke)
      if (!oLTLRealizability.realisable(playerName, goal)) {

        val newModules: Seq[Module] = for (j <- modules.indices) yield {
          if (i == j) modules(j) else strategies(j)
        }

        val newModules2 = Modules(newModules)
        val vars = newModules2.modules.map(_.vars).reduce(_ union _)
        val newArena = Arena("foobar", vars, newModules2)
        val kb = new ScalaxKripkeStructureBuilder
        val newArenaKripke = KripkeSemantics.toKripke(newArena, kb)
        val oLTLRealizability2 = new OLTLRealizability(newArena, newArenaKripke)
        if (oLTLRealizability2.realisable(playerName, goal)) {
          return false
        }
      }

    }
    true
  }

  def main(args: Array[String]): Unit = {
    val example = MutexExample.constructExample(3)
    val strats = MutexExample.constructHoggerStrategyProfile(3)
    val goals = MutexExample.constructNiceGoals(3)
    println(isNE(example, strats, goals))
  }

}