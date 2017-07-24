package gnasher.oltl

import gnasher.pl.PLFormula

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait OLTLFormula

case class Next(e: OLTLFormula) extends OLTLFormula

case class Until(e1: OLTLFormula, e2: OLTLFormula) extends OLTLFormula

case class NonTemporal(e: PLFormula) extends OLTLFormula

object OLTL {

  def orderedSubformulae(formula: OLTLFormula): Seq[OLTLFormula] = {
    val q = mutable.Queue.empty[OLTLFormula]
    val out = ListBuffer.empty[OLTLFormula]
    q.enqueue(formula)
    while (q.nonEmpty) {
      q.dequeue() match {
        case Next(e) =>
          out.prepend(Next(e))
          q.enqueue(e)
        case Until(e1, e2) =>
          out.prepend(Until(e1, e2))
          q.enqueue(e1, e2)
        case NonTemporal(e) =>
          out.prepend(NonTemporal(e))
      }
    }
    out.toList
  }

}