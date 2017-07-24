package gnasher.reactive

import gnasher.kripke.{ScalaxKripkeStructure, ScalaxKripkeStructureBuilder}
import gnasher.pl.{False, Or, True, Var}
import gnasher.reactive.model._
import gnasher.reactive.semantics.KripkeSemantics

object WorstCaseExample {

  private[this] def makePlayerModule(i: Int): Module = {
    val name = s"player$i"
    val vars = Set(s"x$i")
    val inits = makePlayerCommands(i, vars)
    val updates = makePlayerCommands(i, vars)
    Module(name, vars, inits, updates)
  }

  private[this] def makePlayerCommands(i: Int, moduleVars: Set[String]): Seq[Guard] = {
    val xi = s"x$i"
    Seq(
      Guard(True, Seq(Assign(xi, True)), moduleVars),
      Guard(True, Seq(Assign(xi, False)), moduleVars)
    )
  }

  def constructExample(size: Int): Arena = {
    require(size >= 2, "size must be bigger than 2")
    val modules = for (i <- 0 until size) yield makePlayerModule(i)
    val vars = for (i <- 0 until size) yield Set(s"x$i")
    val allVars = vars.reduce(_ union _)
    Arena(s"worst$size", allVars, Modules(modules))
  }

  def main(args: Array[String]): Unit = {

    for (i <- 2 until 400) {
      if (i % 1 == 0) {
        for (r <- 0 until 5) {
          val example = constructExample(i)
          val ksBuilder = new ScalaxKripkeStructureBuilder()
          val t0 = System.nanoTime()
          val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(example, ksBuilder)
          val t1 = System.nanoTime()
          println(s"$i,$r,${t1-t0}")
        }
      }
    }

  }

}

