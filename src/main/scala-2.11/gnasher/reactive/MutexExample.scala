package gnasher.reactive

import gnasher.kripke.{ScalaxKripkeStructure, ScalaxKripkeStructureBuilder}
import gnasher.oltl.{Next, NonTemporal, OLTLFormula, Until}
import gnasher.pl._
import gnasher.reactive.decision.OLTLRealizability
import gnasher.reactive.model._
import gnasher.reactive.semantics.KripkeSemantics

object MutexExample {

  def constructSelfishGoals(n: Int) = {
    val map: Map[String, OLTLFormula] = (for (i <- 0 until n) yield s"player$i" -> Until(NonTemporal(True), NonTemporal(Or(Var(s"x$i"), Var(s"y$i"))))).toMap
    Goals(map)
  }

  def constructNiceGoals(n: Int) = {
    val map: Map[String, OLTLFormula] = (for (i <- 0 until n) yield s"player$i" -> Until(NonTemporal(True), NonTemporal(Var(s"y$i")))).toMap
    Goals(map)
  }

  private[this] def makePlayerModule(i: Int, total: Int): Module = {
    val name = s"player$i"
    val vars = Set(s"x$i", s"y$i")
    val inits = makePlayerInits(i, total, vars)
    val updates = makePlayerUpdates(i, total, vars)
    Module(name, vars, inits, updates)
  }

  private[this] def makePasserStrategy(i: Int, total: Int): Module = {
    val name = s"player$i"
    val vars = Set(s"x$i", s"y$i")
    val inits = makePlayerInits(i, total, vars)
    val updates = makePasserUpdates(i, total, vars)
    Module(name, vars, inits, updates)
  }

  private[this] def makeHoggerStrategy(i: Int, total: Int): Module = {
    val name = s"player$i"
    val vars = Set(s"x$i", s"y$i")
    val inits = makePlayerInits(i, total, vars)
    val updates = makeHoggerUpdates(i, total, vars)
    Module(name, vars, inits, updates)
  }


  private[this] def makePlayerInits(i: Int, total: Int, moduleVars: Set[String]): Seq[Guard] = {
    val isStart = if (i == total-1) True else False
    Seq(Guard(True, Seq(Assign(s"x$i", False), Assign(s"y$i", isStart)), moduleVars))
  }

  private[this] def makePasserUpdates(i: Int, total: Int, moduleVars: Set[String]): Seq[Guard] = {
    val j = (i+total-1) % total

    val xi = s"x$i"
    val yi = s"y$i"
    val xj = s"x$j"
    val yj = s"y$j"

    val first = {
      Guard(
        Var(xi),
        Seq(
          Assign(yi, True),
          Assign(xi, False)
        ),
        moduleVars
      )
    }

    val second = {
      Guard(
        Var(yj),
        Seq(
          Assign(xi, True)
        ),
        moduleVars
      )
    }

    val third = {
      Guard(
        Var(yi),
        Seq(
          Assign(yi, False)
        ),
        moduleVars
      )
    }

    Seq(first, second, third)
  }

  private[this] def makeHoggerUpdates(i: Int, total: Int, moduleVars: Set[String]): Seq[Guard] = {
    val j = (i+total-1) % total

    val xi = s"x$i"
    val yi = s"y$i"
    val xj = s"x$j"
    val yj = s"y$j"


    val first = {
      Guard(
        Or(Var(yj), Var(xi)),
        Seq(
          Assign(xi, True)
        ),
        moduleVars
      )
    }

    val third = {
      Guard(
        Var(yi),
        Seq(
          Assign(yi, False)
        ),
        moduleVars
      )
    }

    Seq(first, third)
  }


  private[this] def makePlayerUpdates(i: Int, total: Int, moduleVars: Set[String]): Seq[Guard] = {
    val j = (i+total-1) % total

    val xi = s"x$i"
    val yi = s"y$i"
    val xj = s"x$j"
    val yj = s"y$j"


    val first = {
      Guard(
        Or(Var(yj), Var(xi)),
        Seq(
          Assign(xi, True)
        ),
        moduleVars
      )
    }

    val second = {
      Guard(
        Or(Var(yj), Var(xi)),
        Seq(
          Assign(xi, False),
          Assign(yi, True)
        ),
        moduleVars
      )
    }

    val third = {
      Guard(
        Var(yi),
        Seq(
          Assign(yi, False)
        ),
        moduleVars
      )
    }

    Seq(first, second, third)
  }

  def constructExample(size: Int): Arena = {
    require(size >= 3, "size must be bigger than 3")
    val modules = for (i <- 0 until size) yield makePlayerModule(i, size)
    val vars = for (i <- 0 until size) yield Set(s"x$i", s"y$i")
    val allVars = vars.reduce(_ union _)
    Arena(s"mutex$size", allVars, Modules(modules))
  }

  def constructPasserStrategyProfile(size: Int): Arena = {
    require(size >= 3, "size must be bigger than 3")
    val modules = for (i <- 0 until size) yield makePasserStrategy(i, size)
    val vars = for (i <- 0 until size) yield Set(s"x$i", s"y$i")
    val allVars = vars.reduce(_ union _)
    Arena(s"mutex-pass$size", allVars, Modules(modules))
  }

  def constructHoggerStrategyProfile(size: Int): Arena = {
    require(size >= 3, "size must be bigger than 3")
    val modules = for (i <- 0 until size) yield makeHoggerStrategy(i, size)
    val vars = for (i <- 0 until size) yield Set(s"x$i", s"y$i")
    val allVars = vars.reduce(_ union _)
    Arena(s"mutex-hog$size", allVars, Modules(modules))
  }


  def main(args: Array[String]): Unit = {

    Thread.sleep(2000)
    val example = constructExample(3)
    val ksBuilder = new ScalaxKripkeStructureBuilder()
    val g = KripkeSemantics.toKripke(example, ksBuilder)
    val oLTLRealizability = new OLTLRealizability(example, g)
    val a = oLTLRealizability.realisable("player0", Until(NonTemporal(True), NonTemporal(Or(Var("x0"), Var("y0")))))
    println(g.exportAsDot("passer3"))
//
//    val example = constructExample(7)
//    val ksBuilder = new ScalaxKripkeStructureBuilder()
//    val g = KripkeSemantics.toKripke(example, ksBuilder)
//    println(g.exportAsDot("mutex5"))

//    for (i <- 240 until 400) {
//      if (i % 15 == 0) {
//        for (r <- 0 until 20) {
//          val example = constructExample(i)
//          val ksBuilder = new ScalaxKripkeStructureBuilder()
//          val t0 = System.nanoTime()
//          val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(example, ksBuilder)
//          val t1 = System.nanoTime()
//          println(s"$i,$r,${t1-t0}")
//        }
//      }
//    }

//    for (i <- 3 until 400) {
//      if (i % 1 == 0) {
//        for (r <- 0 until 10) {
//          val example = constructExample(i)
//          val ksBuilder = new ScalaxKripkeStructureBuilder()
//          val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(example, ksBuilder)
//          val s = g.reachableStates.size
//          val oLTLRealizability = new OLTLRealizability(example, g)
//          val goal = Until(NonTemporal(True), Next(NonTemporal(Or(Var("x0"), Var("y0")))))
//          val player = "player0"
//          val t0 = System.nanoTime()
//          val a = oLTLRealizability.realisable(player, goal)
//          val t1 = System.nanoTime()
//          println(s"$i,$r,$s,$a,${t1-t0}")
//        }
//      }
//    }
//
//    val example = constructHoggerStrategyProfile(5)
//    val ksBuilder = new ScalaxKripkeStructureBuilder()
//    val t0 = System.nanoTime()
//    val g: ScalaxKripkeStructure = KripkeSemantics.toKripke(example, ksBuilder)
//    println(g.exportAsDot("mutex5"))
  }

}
