package gnasher.reactive.parsing

import gnasher.oltl._
import gnasher.pl._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

private[parsing] object Parser extends Parsers {
  import Tokens._

  override type Elem = ReactiveToken

  private class ReactiveTokenReader(tokens: Seq[ReactiveToken]) extends Reader[ReactiveToken] {
    override def first: ReactiveToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[ReactiveToken] = new ReactiveTokenReader(tokens.tail)
  }

  private def ident: Parser[String] = accept("ident", { case IDENT(name) => name })

  private def formula: Parser[PLFormula]  = {
    def expr1: Parser[PLFormula] = (expr2 ~ IFF ~ expr1) ^^ { case f1 ~ _ ~ f2 => Iff(f1, f2) } | expr2
    def expr2: Parser[PLFormula] = (expr3 ~ IMPLIES ~ expr2) ^^ { case f1 ~ _ ~ f2 => Implies(f1, f2) } | expr3
    def expr3: Parser[PLFormula] = (term ~ DISJUNCT ~ expr3) ^^ { case f1 ~ _ ~ f2 => Or(f1, f2) } | term
    def term: Parser[PLFormula] = (notfactor ~ CONJUNCT ~ term) ^^ { case f1 ~ _ ~ f2 => And(f1, f2) } | notfactor
    def notfactor = (NOT ~ factor) ^^ { case _ ~ f => Not(f) } | factor
    def factor = t | f | variable | (LPAR ~ expr1 ~ RPAR) ^^ { case _ ~ f ~ _ => f }
    def t = accept(TRUE) ^^ (_ => True)
    def f = accept(FALSE) ^^ (_ => False)
    def variable = ident ^^ Var

    expr1
  }

  private def oltlFormula: Parser[OLTLFormula] = {
    def exprNonTemporal: Parser[OLTLFormula] = formula ^^ { case f => NonTemporal(f) }

    def exprUntil: Parser[OLTLFormula] = (exprFinally ~ UNTIL ~ exprUntil) ^^ { case f1 ~ _ ~ f2 => Until(f1, f2) } | exprFinally

    def exprFinally: Parser[OLTLFormula] = (FINALLY ~ exprNext) ^^ { case _  ~ f1 => Until(NonTemporal(True), f1) } | exprNext

    def exprNext: Parser[OLTLFormula] = (NEXT ~ exprNext) ^^ { case _ ~ f1 => Next(f1) } | exprNonTemporal

    exprUntil
  }

  private def identList: Parser[Seq[String]] = rep1sep(ident, accept(COMMA))

  private def assign: Parser[AssignAST] = {
    (ident ~ ASSIGN ~ formula) ^^ {
      case id ~ _ ~ form => AssignAST(id, form)
    }
  }

  private def assigns: Parser[Seq[AssignAST]] = rep1sep(assign, accept(SEMICOLON))

  private def guard: Parser[GuardAST] = {
    (formula ~ GUARD ~ assigns) ^^ {
      case cond ~ _ ~ assignments => GuardAST(cond, assignments)
    }
  }

  private def initGuard: Parser[GuardAST] = {
    (GUARD ~ assigns) ^^ {
      case _ ~ assigns => GuardAST(True, assigns)
    }
  }

  private def guards: Parser[Seq[GuardAST]] = rep1sep(guard, accept(SEP))

  private def initGuards: Parser[Seq[GuardAST]] = rep1sep(initGuard, accept(SEP))

  private def module: Parser[ModuleAST] = {
    (MODULE ~ ident ~ CONTROLS ~ identList ~ INIT ~ initGuards ~ UPDATE ~ guards ~ ENDMODULE) ^^ {
      case _ ~ name ~ _ ~ vars ~ _ ~ inits ~ _ ~ updates ~ _ => ModuleAST(name, vars, inits, updates)
    }
  }

  private def strategy: Parser[StrategyAST] = {
    (STRATEGY ~ ident ~ CONTROLS ~ identList ~ INIT ~ initGuards ~ UPDATE ~ guards ~ ENDSTRATEGY) ^^ {
      case _ ~ name ~ _ ~ vars ~ _ ~ inits ~ _ ~ updates ~ _ => StrategyAST(name, vars, inits, updates)
    }
  }

  private def modules: Parser[Seq[ModuleAST]] = rep1(module)

  private def strategies: Parser[Seq[StrategyAST]] = rep1(strategy)

  private def arena: Parser[ArenaAST] = {
    (ARENA ~ ident ~ CONTROLS ~ identList ~ modules ~ ENDARENA) ^^ {
      case _ ~ name ~ _ ~ vars ~ modules ~ _ => ArenaAST(name, vars, modules)
    }
  }


  private def profile: Parser[ProfileAST] = {
    (PROFILE ~ ident ~ CONTROLS ~ identList ~ strategies ~ ENDPROFILE) ^^ {
      case _ ~ name ~ _ ~ vars ~ strategies ~ _ => ProfileAST(name, vars, strategies)
    }
  }

  private def goals: Parser[GoalsAST] = {
    (GOALS ~ goalList ~ ENDGOALS) ^^ {
      case _ ~ goalList ~ _ => GoalsAST(goalList)
    }
  }

  private def goalList: Parser[Seq[GoalAST]] = rep1sep(goal, accept(SEMICOLON))

  private def goal: Parser[GoalAST] = {
    (ident ~ ASSIGN ~ oltlFormula) ^^ {
      case player ~ _ ~ goal => GoalAST(player, goal)
    }
  }

  def apply(tokens: Seq[ReactiveToken]): ReactiveAST = {
    val reader = new ReactiveTokenReader(tokens)
    arena(reader) match {
      case NoSuccess(msg, _) => throw ReactiveParserException(msg)
      case Success(result, _) => result
    }
  }

  def parseArena(tokens: Seq[ReactiveToken]): ArenaAST = {
    val reader = new ReactiveTokenReader(tokens)
    arena(reader) match {
      case NoSuccess(msg, _) => throw ReactiveParserException(msg)
      case Success(result, _) => result
    }
  }

  def parseGoals(tokens: Seq[ReactiveToken]): GoalsAST = {
    val reader = new ReactiveTokenReader(tokens)
    goals(reader) match {
      case NoSuccess(msg, _) => throw ReactiveParserException(msg)
      case Success(result, _) => result
    }
  }

  def parseProfile(tokens: Seq[ReactiveToken]): ProfileAST = {
    val reader = new ReactiveTokenReader(tokens)
    profile(reader) match {
      case NoSuccess(msg, _) => throw ReactiveParserException(msg)
      case Success(result, _) => result
    }
  }

}