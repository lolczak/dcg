package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.{Production, Term, FeatureStruct, FValue}

case class VarAssignments(assignments: Map[String, FValue]) {

  def +(varName: String, value: FValue): VarAssignments = ???

  def combine(that: VarAssignments): Option[VarAssignments] = ???

}

object VarAssignments {

  val empty = VarAssignments(Map.empty)

  def fromFStruct(featureStruct: FeatureStruct): VarAssignments = ???

  def fromRule(production: Production, parsedTerms: List[Term]):  Option[VarAssignments] = ???

}

