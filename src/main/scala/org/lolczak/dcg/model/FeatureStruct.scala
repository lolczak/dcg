package org.lolczak.dcg.model

import org.lolczak.dcg.parser.language.variable.VariableAssignment

case class FeatureStruct(features: Map[String, FeatureRhsOperand]) {

  def apply(featName: String):Option[FeatureRhsOperand] = features.get(featName)

  val containsVariables = features.exists(_._2.isVariable)

  def substitute(substitution: VariableAssignment): FeatureStruct = {
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
