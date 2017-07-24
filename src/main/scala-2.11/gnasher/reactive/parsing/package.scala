package gnasher.reactive

import gnasher.reactive.model.Arena

package object parsing {

  abstract class ReactivePipelineException(msg: String) extends Exception(msg)
  case class ReactiveLexerException(msg: String) extends ReactivePipelineException(msg)
  case class ReactiveParserException(msg: String) extends  ReactivePipelineException(msg)
  case class ReactiveCheckerException(msg: String) extends ReactivePipelineException(msg)

  def parseArena(source: String): Arena = Checker(Parser(Lexer(source)))

  def parseGoals(source: String): GoalsAST = Parser.parseGoals(Lexer(source))

}
