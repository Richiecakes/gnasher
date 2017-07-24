package gnasher.pl

case class ValuationImpl(vars: Set[String], domain: Set[String]) extends Valuation {
  override def lookup(variable: String): Boolean =
    vars.contains(variable)

  override def union(other: Valuation): Valuation =
    ValuationImpl(vars.union(other.vars), domain.union(other.domain))

  override def restrict(newDomain: Set[String]): Valuation =
    ValuationImpl(vars.intersect(newDomain), newDomain)

  override def \(vars2: Set[String]): Valuation =
    ValuationImpl(vars -- vars2, domain)

  override def set(variable: String): Valuation =
    ValuationImpl(vars + variable, domain + variable)

  override def set(variables: Set[String]): Valuation =
    ValuationImpl(vars ++ variables, domain ++ variables)

  override def models(formula: PLFormula): Boolean = formula match {
    case And(f1, f2) => models(f1) && models(f2)
    case Or(f1, f2) => models(f1) || models(f2)
    case Not(f) => !models(f)
    case Implies(f1, f2) => !models(f1) || models(f2)
    case Iff(f1, f2) => models(f1) == models(f2)
    case True => true
    case False => false
    case Var(name) => lookup(name)
  }

  override def \(variable: String): Valuation =
    ValuationImpl(vars - variable, domain)
}

object ValuationImpl {
  def empty(domain: Set[String]): ValuationImpl = ValuationImpl(Set.empty, domain)
}