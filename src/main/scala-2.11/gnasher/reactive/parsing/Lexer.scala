package gnasher.reactive.parsing

import scala.util.parsing.combinator.RegexParsers

private[parsing] object Lexer extends RegexParsers {
  import Tokens._

  override val skipWhitespace: Boolean = true

  private def ident: Parser[IDENT] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r  ^^ { IDENT }
  }

  private val keywordTable = Seq (
    ("module", MODULE),
    ("strategy", STRATEGY),
    ("arena", ARENA),
    ("profile", PROFILE),
    ("goals", GOALS),
    ("endmodule", ENDMODULE),
    ("endstrategy", ENDSTRATEGY),
    ("endarena", ENDARENA),
    ("endprofile", ENDPROFILE),
    ("endgoals", ENDGOALS),
    ("controls", CONTROLS),
    ("init", INIT),
    ("update", UPDATE),
    (":=", ASSIGN),
    ("-->", GUARD),
    ("true", TRUE),
    ("false", FALSE),
    ("|", DISJUNCT),
    ("&", CONJUNCT),
    ("~", NOT),
    ("=>", IMPLIES),
    ("<=>", IFF),
    ("(", LPAR),
    (")", RPAR),
    (",", COMMA),
    (";", SEMICOLON),
    ("::", SEP),
    ("X", NEXT),
    ("U", UNTIL),
    ("F", FINALLY)
  )

  private def keywords: Seq[Parser[ReactiveToken]] = keywordTable.map {
    case (keyword, token) => keyword ^^ (_ => token)
  }

  private def tokens: Parser[Seq[ReactiveToken]] = {
    val all = keywords ++ Seq(ident)
    phrase(rep1(all.reduce(_ | _)))
  }

  def apply(code: String): Seq[ReactiveToken] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => throw ReactiveLexerException(msg)
      case Success(result, next) => result
    }
  }

}
