package gnasher.pl

trait Valuation {
  def vars: Set[String]
  def domain: Set[String]
  def lookup(variable: String): Boolean
  def union(other: Valuation): Valuation
  def restrict(newDomain: Set[String]): Valuation
  def \(vars2: Set[String]): Valuation
  def \(variable: String): Valuation
  def set(variable: String): Valuation
  def set(variables: Set[String]): Valuation

  def models(formula: PLFormula): Boolean
  def |=(formula: PLFormula): Boolean = models(formula)

  def label: String = vars.mkString("[", ",", "]")
}

object Valuation {
  def empty(domain: Set[String]): Valuation = ValuationImpl.empty(domain)
  def union(valuations: Seq[Valuation]): Valuation = valuations.reduce(_.union(_))
}