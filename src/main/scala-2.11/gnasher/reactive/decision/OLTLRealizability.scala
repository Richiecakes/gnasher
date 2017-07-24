package gnasher.reactive.decision

import gnasher.kripke.KripkeStructure
import gnasher.oltl._
import gnasher.pl.Valuation
import gnasher.reactive.model.{Arena, Guard, JointGuard}

import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

class OLTLRealizability(private val arena: Arena,
                        private val ks: KripkeStructure) {

  private val labels: MutableMap[Valuation, MutableSet[(String, OLTLFormula)]] = mutable.HashMap.empty

  private val completed: MutableSet[(String, OLTLFormula)] = mutable.HashSet.empty

  private val reachableValuations = ks.reachableStates.map(_.valuation)

  def clearLabelCache() = {
    reachableValuations.foreach { v => labels.put(v, MutableSet.empty) }
  }

  clearLabelCache()

  def realisable(player: String, goal: OLTLFormula): Boolean = {
    ensureNecessaryLabels(player, goal)

    val jointInits = arena.jointInits
    val groupedByPlayer: Map[Guard, Seq[JointGuard]] =
      jointInits.groupBy(_.guards(player))

    val emptyValuation = Valuation.empty(arena.vars)

    groupedByPlayer.values.exists { jointInits => // Exists initialisation choice of this player...
      val forcedStates = jointInits.map(_.exec(emptyValuation))
      forcedStates.forall { v => // Such that for all resulting initialisations...
        labels(v).contains((player, goal))             // The goal is achieved.
      }
    }
  }

  private def ensureNecessaryLabels(player: String, goal: OLTLFormula) = {

    def can(nextGoal: OLTLFormula, valuation: Valuation): Boolean = {
      val enabled: Seq[JointGuard] = arena.enabled(valuation)

      val groupedByPlayer: Map[Guard, Seq[JointGuard]] =
        enabled.groupBy(_.guards(player))

      groupedByPlayer.values.exists { jointUpdates => // Exists choice of this player...
        val forcedStates = jointUpdates.map(_.exec(valuation))
        forcedStates.forall { v => // Such that for all possible steps...
          labels(v).contains((player, nextGoal))             // The goal is achieved.
        }
      }

    }

    val subgoals = OLTL.orderedSubformulae(goal)
    for (subgoal <- subgoals if !completed.contains((player, subgoal))) {
      val realisabilityCondition = subgoal match {
        case Next(e) =>
          v: Valuation => can(e, v)
        case Until(e1, e2) =>
          v: Valuation => labels(v).contains((player, e2)) || (labels(v).contains((player, e1)) && can(subgoal, v))
        case NonTemporal(e) =>
          v: Valuation => v |= e
      }

      var changed = false
      do {
        changed = false
        val subgoalRealisable = reachableValuations.filter(realisabilityCondition)
        for (v <- subgoalRealisable) {
          val isNewLabel = labels(v).add((player, subgoal))
          changed = changed || isNewLabel
        }
      } while (changed)
      completed.add((player, subgoal))
    }
  }
}
