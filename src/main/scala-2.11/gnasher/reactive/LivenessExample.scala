package gnasher.reactive

import gnasher.kripke.{ScalaxKripkeStructure, ScalaxKripkeStructureBuilder}
import gnasher.oltl._
import gnasher.pl._
import gnasher.reactive.decision.OLTLRealizability
import gnasher.reactive.model._
import gnasher.reactive.semantics.KripkeSemantics

object LivenessExample {

  def makePlayerInits(i: Int, vars: Set[String]): Seq[Guard] = Seq(
    Guard(True, Seq(Assign(s"x$i", False)), vars)
  )

  def makePlayerUpdates(i: Int, vars: Set[String]): Seq[Guard] = {
    val cond = if (i == 0) Not(Var(s"x$i")) else And(Var(s"x${i-1}"), Not(Var(s"x$i")))
    Seq(
      Guard(cond, Seq(Assign(s"x$i", True)), vars),
      Guard(cond, Seq(), vars)
    )
  }

  def constructGoals(n: Int) = {
    val map: Map[String, OLTLFormula] = (for (i <- 0 until n) yield s"player$i" -> Until(NonTemporal(True), NonTemporal(Var(s"x${n-1}")))).toMap
    Goals(map)
  }

  def makePlayerModule(i: Int, size: Int): Module = {
    val name = s"player$i"
    val vars = Set(s"x$i")
    val inits = makePlayerInits(i, vars)
    val updates = makePlayerUpdates(i, vars)
    Module(name, vars, inits, updates)
  }

  def constructExample(size: Int): Arena = {
    require(size >= 2, "size must be bigger than 2")
    val modules = for (i <- 0 until size) yield makePlayerModule(i, size)
    val vars = (for (i <- 0 until size) yield s"x$i").toSet
    Arena(s"live$size", vars, Modules(modules))
  }

  def main(args: Array[String]): Unit = {
    val example = constructExample(7)
    val ksBuilder = new ScalaxKripkeStructureBuilder()
    val g = KripkeSemantics.toKripke(example, ksBuilder)
    println(g.exportAsDot("crew7"))
//    val oLTLRealizability = new OLTLRealizability(example, g)
//    val ans = oLTLRealizability.realisable("player0", finishGoal)
//    println(ans)
//
//    for (i <- 15 until 400) {
//      if (i % 15 == 0) {
//        for (r <- 0 until 5) {
//          val example = constructExample(i)
//          val ksBuilder = new ScalaxKripkeStructureBuilder()
//          val t0 = System.nanoTime()
//          val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(example, ksBuilder)
//          val t1 = System.nanoTime()
//          println(s"$i,$r,${t1-t0}")
//        }
//      }
//    }
  }

}
