package org.lolczak.dcg

import org.lolczak.dcg.parser.language.variable.Substitution

import scala.language.implicitConversions

case class Grammar(start: String, productions: List[Production]) {

  private val prefixes: Map[String, Set[Production]] = productions.map(p => (p.rhs.head.name, p)).groupBy(_._1).mapValues(t => Set(t.map(_._2): _*)).withDefaultValue(Set.empty)

  def findStartingWith(symbol: String): Set[Production] = prefixes(symbol)

}

case class Production(lhs: Term, rhs: List[Term])

case class LexProduction(lhs: Term, rhs: List[String])

case class Term(name: String, fStruct: FeatureStruct = FeatureStruct.empty) {

  override def toString: String = name + fStruct.toString

}

//todo extract to separate file
case class FeatureStruct(features: Map[String, FeatureRhsOperand]) {

  def apply(featName: String):Option[FeatureRhsOperand] = features.get(featName)

  val containsVariables = features.exists(_._2.isVariable)

  def substitute(substitution: Substitution): FeatureStruct = {
    val substituted = features.mapValues {
      case v@FVariable(name) => substitution.find(name).getOrElse(v)
      case x => x
    }
    FeatureStruct(substituted)
  }

  override def toString: String =
    if (features.isEmpty) ""
    else features.mkString("[", ",", "]")

}

sealed trait FeatureRhsOperand {
  val isVariable: Boolean
}
case class FVariable(name: String) extends FeatureRhsOperand {
  override val isVariable: Boolean = true

  override def toString: String = s"?$name"
}
sealed trait FeatureValue extends FeatureRhsOperand
case class FConst(value: String) extends FeatureValue {
  override val isVariable: Boolean = false

  override def toString: String = value
}
case class FList(elements: List[FeatureRhsOperand]) extends FeatureValue {
  override val isVariable: Boolean = false

  override def toString: String = elements.mkString("<", ",", ">")
}

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
    def apply(features: (String, FeatureRhsOperand)*) = Term(name, FeatureStruct(Map(features: _*)))
  }

}