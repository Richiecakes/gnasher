package gnasher.reactive.parsing

import gnasher.oltl.OLTLFormula
import gnasher.pl.PLFormula

private[parsing] sealed trait ReactiveAST

private[parsing] case class GoalsAST(goals: Seq[GoalAST]) extends ReactiveAST

private[parsing] case class GoalAST(player: String, goal: OLTLFormula) extends ReactiveAST

private[parsing] case class ArenaAST(name: String, vars: Seq[String], modules: Seq[ModuleAST]) extends ReactiveAST {
  override def toString: String =
    s"""
       |Arena($name)(${vars.mkString(", ")}) {
       |  ${modules.mkString("\n")}
       |}
  """.stripMargin
}

private[parsing] case class ProfileAST(name: String, vars: Seq[String], strategies: Seq[StrategyAST]) extends ReactiveAST {
  override def toString: String =
    s"""
       |Profile($name)(${vars.mkString(", ")}) {
       |  ${strategies.mkString("\n")}
       |}
  """.stripMargin
}

private[parsing] case class ModuleAST(name: String, vars: Seq[String], inits: Seq[GuardAST], updates: Seq[GuardAST]) extends ReactiveAST {
  override def toString: String =
    s"""
       |Module($name)(${vars.mkString(", ")}) {
       |  Init:
       |    ${inits.mkString("\n")}
       |  Update:
       |    ${updates.mkString("\n    ")}
       |}
  """.stripMargin
}

private[parsing] case class StrategyAST(name: String, vars: Seq[String], inits: Seq[GuardAST], updates: Seq[GuardAST]) extends ReactiveAST {
  override def toString: String =
    s"""
       |Strategy($name)(${vars.mkString(", ")}) {
       |  Init:
       |    ${inits.mkString("\n")}
       |  Update:
       |    ${updates.mkString("\n    ")}
       |}
  """.stripMargin
}

private[parsing] case class GuardAST(condition: PLFormula, assignments: Seq[AssignAST]) extends ReactiveAST
private[parsing] case class AssignAST(varName: String, formula: PLFormula) extends ReactiveAST