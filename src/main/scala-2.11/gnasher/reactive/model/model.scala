package gnasher.reactive.model

import gnasher.Util
import gnasher.oltl.OLTLFormula
import gnasher.pl._

import scala.collection.mutable

sealed trait ReactiveModel

case class Goals(goals: Map[String, OLTLFormula]) extends ReactiveModel

case class Arena(name: String, vars: Set[String], modules: Modules) extends ReactiveModel {
  lazy val jointInits: Seq[JointGuard] = {
    val inits: Map[String, Seq[Guard]] = modules.inits
    val jointInits = transposeMap(inits)
    jointInits.map(JointGuard.apply)
  }

  private[this] val cache: scala.collection.mutable.Map[Valuation, Seq[JointGuard]] = new mutable.HashMap()

  def enabled(valuation: Valuation): Seq[JointGuard] = {
    if (cache.contains(valuation)) return cache(valuation)
    val enabledUpdates = modules.enabledUpdates(valuation)
    val jointEnabledUpdates = transposeMap(enabledUpdates)
    val js = jointEnabledUpdates.map(JointGuard.apply)
    cache(valuation) = js
    js
  }

  private def transposeMap[A, B](x: Map[A, Seq[B]]): Seq[Map[A, B]] = {
    val xss = x.values.toSeq
    val keys = x.keys
    val yss = Util.cartesianProduct(xss)
    yss.map(ys => (keys zip ys).toMap)
  }
}

case class Modules(modules: Seq[Module]) extends ReactiveModel {
  lazy val modulesMap = modules.map(m => m.name -> m).toMap

  lazy val inits: Map[String, Seq[Guard]] =
    modulesMap.mapValues(m => m.inits)

  lazy val updates: Map[String, Seq[Guard]] =
    modulesMap.mapValues(m => m.updates)

  def enabledUpdates(valuation: Valuation): Map[String, Seq[Guard]] =
    modulesMap.map { case (k, m) => (k, m.enabledUpdates(valuation)) }
}

case class JointGuard(guards: Map[String, Guard]) {
  def exec(valuation: Valuation): Valuation = {
    var newValuation = valuation
    for (guard <- guards.values; assignment <- guard.assignments) {
      if (valuation |= assignment.formula) newValuation  = newValuation.set(assignment.varName)
      else newValuation = newValuation \ assignment.varName
    }
    newValuation
  }
}

case class Module(name: String, vars: Set[String], inits: Seq[Guard], updates: Seq[Guard]) extends ReactiveModel {
  def enabledUpdates(valuation: Valuation): Seq[Guard] = {
    val enabled = updates.filter(g => valuation |= g.condition)
    if (enabled.isEmpty) Seq(skip) else enabled
  }

  lazy val skip: Guard = Guard(True, Seq.empty, vars)
}

case class Guard(condition: PLFormula, assignments: Seq[Assign], moduleVars: Set[String]) extends ReactiveModel {
  lazy val controlledVars = assignments.map(_.varName).toSet

  def exec(valuation: Valuation): Valuation = {
    val updated =
      for {
        assignment <- assignments
        if valuation |= assignment.formula
      } yield assignment.varName

    val unchanged = valuation.restrict(moduleVars) \ controlledVars
    unchanged.set(updated.toSet)
  }
}

case class Assign(varName: String, formula: PLFormula) extends ReactiveModel