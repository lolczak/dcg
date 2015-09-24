package org.lolczak.dcg.model

import scala.language.implicitConversions

case class Nonterminals(start: String, productions: List[Production]) {

  private val prefixes: Map[String, Set[Production]] = productions.map(p => (p.rhs.head.name, p)).groupBy(_._1).mapValues(t => Set(t.map(_._2): _*)).withDefaultValue(Set.empty)

  def findStartingWith(symbol: String): Set[Production] = prefixes(symbol)

}

case class Production(lhs: Term, rhs: List[Term], maybeSnippet: Option[String] = None) {
  val containsGuard:Boolean = maybeSnippet.isDefined
}

case class TerminalProduction(lhs: Term, rhs: List[String])

case class Term(name: String, fStruct: FeatureStruct = FeatureStruct.empty) {

  override def toString: String = name + fStruct.toString

}

case class ImportDirective(file: String)

object Nonterminals {

  implicit def string2Term(symbol: String): Term = Term(symbol, FeatureStruct.empty)

  implicit class LhsString(term: String) {
    def ~>(rhs: String*): Production = Production(term, List(rhs: _*) map string2Term)
  }

  implicit class LhsTerm(term: Term) {
    def ~>(rhs: Term*): Production = Production(term, List(rhs: _*))
  }

  implicit class TermString(name: String) {
    def apply(features: (String, FeatureRhsOperand)*) = Term(name, FeatureStruct(Map(features: _*)))
  }

}