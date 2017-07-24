package gnasher.reactive.parsing

private[parsing] sealed trait ReactiveToken

private[parsing] object Tokens {
  case object MODULE extends ReactiveToken
  case object STRATEGY extends ReactiveToken
  case object ARENA extends ReactiveToken
  case object PROFILE extends ReactiveToken
  case object GOALS extends ReactiveToken
  case object ENDMODULE extends ReactiveToken
  case object ENDSTRATEGY extends ReactiveToken
  case object ENDARENA extends ReactiveToken
  case object ENDPROFILE extends ReactiveToken
  case object ENDGOALS extends ReactiveToken
  case object CONTROLS extends ReactiveToken
  case object INIT extends ReactiveToken
  case object UPDATE extends ReactiveToken
  case object ASSIGN extends ReactiveToken
  case object GUARD extends ReactiveToken
  case object TRUE extends ReactiveToken
  case object FALSE extends ReactiveToken
  case object DISJUNCT extends ReactiveToken
  case object CONJUNCT extends ReactiveToken
  case object NOT extends ReactiveToken
  case object IMPLIES extends ReactiveToken
  case object IFF extends ReactiveToken
  case object LPAR extends ReactiveToken
  case object RPAR extends ReactiveToken
  case object COMMA extends ReactiveToken
  case object SEMICOLON extends ReactiveToken
  case object SEP extends ReactiveToken

  // LTL stuff
  case object UNTIL extends ReactiveToken
  case object NEXT extends ReactiveToken
  case object FINALLY extends ReactiveToken

  case class IDENT(label: String) extends ReactiveToken
}

