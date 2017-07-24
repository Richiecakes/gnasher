package gnasher.reactive

import gnasher.kripke.{ScalaxKripkeStructure, ScalaxKripkeStructureBuilder}
import gnasher.oltl._
import gnasher.pl._
import gnasher.reactive.decision.OLTLRealizability
import gnasher.reactive.model._
import gnasher.reactive.semantics.KripkeSemantics

object PhilExample {

  def makePlayerInits(i: Int, vars: Set[String]): Seq[Guard] = Seq(
    Guard(
      True,
      Seq(
        Assign(s"l$i", False),
        Assign(s"r$i", False),
        Assign(s"L$i", False),
        Assign(s"R$i", False),
        Assign(s"e$i", False)
      ),
      vars
    )
  )

  def makePlayerUpdates(i: Int, n: Int, vars: Set[String]): Seq[Guard] = {
    val li = s"l$i"
    val ri = s"r$i"
    val Li = s"L$i"
    val Ri = s"R$i"
    val ei = s"e$i"

    val left = (i+1)%n
    val right = (i + n-1)%n

    val lj = s"r$left"
    val rj = s"l$right"
    val Lj = s"R$left"
    val Rj = s"L$right"

    implicit def toVar(s: String): PLFormula = Var(s)

    def and(fs: PLFormula*) = fs.fold(True) { case (f1, f2) => And(f1, f2) }

    Seq(
      Guard(Not(li), Seq(Assign(li, True)), vars),
      Guard(Not(li), Seq(), vars),
      Guard(and(li, Not(Li), Not(lj)), Seq(Assign(Li, True)), vars),
      Guard(and(li, Not(Li)), Seq(Assign(li, False)), vars),
      Guard(and(Li, Not(ri)), Seq(Assign(ri, True)), vars),
      Guard(and(Li, Not(ri)), Seq(), vars),
      Guard(and(ri, Not(Ri), Not(rj)), Seq(Assign(Ri, True)), vars),
      Guard(and(ri, Not(Ri)), Seq(Assign(ri, False)), vars),
      Guard(and(Ri, Not(ei)), Seq(Assign(ei, True)), vars),
      Guard(ei, Seq(
        Assign(li, False),
        Assign(ri, False),
        Assign(Li, False),
        Assign(Ri, False),
        Assign(ei, False)
      ), vars)
    )
  }

  def constructGoals(n: Int) = {
    ???
  }

  def makePlayerModule(i: Int, size: Int): Module = {
    val name = s"player$i"
    val vars = Set(s"l$i", s"r$i", s"L$i", s"R$i", s"e$i")
    val inits = makePlayerInits(i, vars)
    val updates = makePlayerUpdates(i, size, vars)
    Module(name, vars, inits, updates)
  }

  def constructExample(size: Int): Arena = {
    require(size >= 2, "size must be bigger than 2")
    val modules = for (i <- 0 until size) yield makePlayerModule(i, size)
    val vars = (for (i <- 0 until size) yield s"x$i").toSet
    Arena(s"live$size", vars, Modules(modules))
  }

  def main(args: Array[String]): Unit = {
    val example = constructExample(3)
    val ksBuilder = new ScalaxKripkeStructureBuilder()
    val g = KripkeSemantics.toKripke(example, ksBuilder)
    println(g.exportAsDot("live5"))
    val oLTLRealizability = new OLTLRealizability(example, g)
    val a = oLTLRealizability.realisable("player0", Until(NonTemporal(True), NonTemporal(Var("e0"))))
    println(a)
  }

}
