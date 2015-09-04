package org.lolczak.dcg

import scala.language.implicitConversions

case class Grammar(start: String, productions: List[Production]) {

  private val prefixes: Map[String, Set[Production]] = productions.map(p => (p.rhs.head.name, p)).groupBy(_._1).mapValues(t => Set(t.map(_._2): _*)).withDefaultValue(Set.empty)

  def findStartingWith(symbol: String): Set[Production] = prefixes(symbol)

}

case class Production(lhs: Term, rhs: List[Term])

case class LexProduction(lhs: Term, rhs: List[String])

case class Term(name: String, fStruct: FeatureStruct = FeatureStruct.empty) {

  def matches(that: Term): Boolean =
    this.name == that.name && (this.fStruct matches that.fStruct)

}

case class FeatureStruct(features: Map[String, FValue]) {

  def matches(that: FeatureStruct): Boolean = {
    val keys = this.features.keySet
    val isConsistent: (FValue, FValue) => Boolean = {
      case (FConst(c1), FConst(c2)) => c1 == c2
      case _ => true
    }
    keys.forall { name =>
      val consistent = for {
        v1 <- this.features.get(name)
        v2 <- that.features.get(name)
      } yield isConsistent(v1, v2)
      consistent.getOrElse(true)
    }
  }

}

sealed trait FValue
case class FConst(value: String) extends FValue
case class FVariable(name: String) extends FValue
case class FList(elements: List[FValue]) extends FValue

object FeatureStruct {
  val empty = FeatureStruct(Map.empty)
}

object Grammar {

  implicit def string2Term(symbol: String): Term = Term(symbol, FeatureStruct.empty)

  implicit class LhsString(term: String) {
    def ~>(rhs: String*): Production = Production(term, List(rhs: _*) map string2Term)
  }

  implicit class LhsTerm(term: Term) {
    def ~>(rhs: Term*): Production = Production(term, List(rhs: _*))
  }

  implicit class TermString(name: String) {
    def apply(features: (String, FValue)*) = Term(name, FeatureStruct(Map(features: _*)))
  }

}