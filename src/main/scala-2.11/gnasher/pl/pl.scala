package gnasher.pl

sealed trait PLFormula

case class And(f1: PLFormula, f2: PLFormula) extends PLFormula
case class Or(f1: PLFormula, f2: PLFormula) extends PLFormula
case class Not(f: PLFormula) extends PLFormula
case class Implies(f1: PLFormula, f2: PLFormula) extends PLFormula
case class Iff(f1: PLFormula, f2: PLFormula) extends PLFormula

case object True extends PLFormula
case object False extends PLFormula
case class Var(name: String) extends PLFormula