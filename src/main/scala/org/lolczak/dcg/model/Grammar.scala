package org.lolczak.dcg.model

import org.lolczak.dcg.parser.language.FeatureAgreement

import scala.language.implicitConversions

case class Grammar(nonterminals:     Nonterminals,
                   lexicon:          Lexicon,
                   importDirectives: List[ImportDirective] = List.empty)

case class Nonterminals(start: String, productions: List[Production]) {

  val emptyTerms: List[Term] = productions.filter(_.isEmpty).map(_.lhs)

  private val index: Map[String, Set[(Production, List[(Term, Term)])]] = buildIndex

  private def buildIndex: Map[String, Set[(Production, List[(Term, Term)])]] = {
    //none of the rules can have only empty terms
    val productionsWithEmptyPrefix: List[(Production, List[(Term, Term)])] = productions.filter(!_.isEmpty).map(x=> (x, x.findEmptyPrefix(emptyTerms)))
    val entries: List[(String, Production, List[(Term, Term)])] = productionsWithEmptyPrefix.map(x=> (x._1.rhs(x._2.size).name, x._1, x._2))

    entries.groupBy(_._1).mapValues(_.map(x=>(x._2, x._3)).toSet).withDefaultValue(Set.empty)
  }

  def findPrefix(symbol: String): Set[(Production, List[(Term, Term)])] = index(symbol)

}

case class Production(lhs: Term, rhs: List[Term], maybeSnippet: Option[String] = None, id: Option[String] = None) {

  val containsGuard: Boolean = maybeSnippet.isDefined

  val isEmpty: Boolean = rhs.isEmpty


  /**
   *
   * @param emptyTerms
   * @return first is prod term, second is empty term
   */
  def findEmptyPrefix(emptyTerms: List[Term]): List[(Term, Term)] = {
    val prefix = rhs.takeWhile(x => emptyTerms.exists(x matches _))
    prefix.map(x=> (x, emptyTerms.find(x matches _).get))
  }

}

case class TerminalProduction(lhs: Term, rhs: List[String])

case class Term(name: String, fStruct: FeatureStruct = FeatureStruct.empty) {

  def matches(that: Term): Boolean = FeatureAgreement.isConsistent(this, that) //todo refactor it

  override def toString: String = name + fStruct.toString

}

case class ImportDirective(file: String)

object Grammar {

  implicit def string2Term(symbol: String): Term = Term(symbol, FeatureStruct.empty)

  implicit class LhsString(term: String) {
    def ~>(rhs: String*): Production = Production(term, List(rhs: _*) map string2Term, None)
  }

  implicit class LhsTerm(term: Term) {
    def ~>(rhs: Term*): Production = Production(term, List(rhs: _*), None)
  }

  implicit class TermString(name: String) {
    def apply(features: (String, FeatureItem)*) = Term(name, FeatureStruct(Map(features: _*)))
  }

}