package gnasher.reactive

import gnasher.kripke.{ScalaxKripkeStructure, ScalaxKripkeStructureBuilder}
import gnasher.oltl._
import gnasher.pl._
import gnasher.reactive.decision.OLTLRealizability
import gnasher.reactive.parsing._
import gnasher.reactive.semantics.KripkeSemantics

object Test {
  def main(args: Array[String]): Unit = {
//    import Examples.mutexGoals1
//    val x = parseGoals(mutexGoals1)
//    println("foo")

    mutexExample()
  }

  def mutexExample() = {
    import Examples.mutex
    val arena = parseArena(mutex)
    val ksBuilder = new ScalaxKripkeStructureBuilder()
    val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(arena, ksBuilder)
    val out = g.exportAsDot("mutex")
    val goal = Until(NonTemporal(True), NonTemporal(Var("x0"))) // Eventually hold mutex
    val goal2 = Until(NonTemporal(True), goal)
    val oLTLRealizability = new OLTLRealizability(arena, g)
    val a1 = oLTLRealizability.realisable("rbme0", goal)
    val a2 = oLTLRealizability.realisable("rbme0", goal2)

    println(arena)
    println()
    println(out)
    println()
    println(a1, a2)
  }

  def mutexWithButlerExample() = {
    import Examples.mutexWithButler
    val arena = parseArena(mutexWithButler)
    val ksBuilder = new ScalaxKripkeStructureBuilder()
    val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(arena, ksBuilder)
    val out = g.exportAsDot("mutex")
    val goal = Until(NonTemporal(True), NonTemporal(Var("y0"))) // Eventually hold mutex
    val oLTLRealizability = new OLTLRealizability(arena, g)
    val a1 = oLTLRealizability.realisable("butler", goal)

    println(arena)
    println()
    println(out)
    println()
    println(a1)
  }


  def rpsExample() = {
    import Examples.rps
    val arena = parseArena(rps)
    val ksBuilder = new ScalaxKripkeStructureBuilder()
    val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(arena, ksBuilder)
    val out = g.exportAsDot("rps")
    val goal = Until(NonTemporal(True), NonTemporal(Var("w1")))
    val oLTLRealizability = new OLTLRealizability(arena, g)
    val a1 = oLTLRealizability.realisable("rps1", goal)

    println(arena)
    println()
    println(out)
    println()
    println(a1)
  }

  implicit def toNonTemp(formula: PLFormula): OLTLFormula = NonTemporal(formula)

}

