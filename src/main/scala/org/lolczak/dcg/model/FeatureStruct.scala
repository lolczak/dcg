package org.lolczak.dcg.model

import org.lolczak.dcg.parser.language.variable.VariableAssignment

case class FeatureStruct(features: Map[String, FeatureRhsOperand]) {

  def apply(featName: String): Option[FeatureRhsOperand] = features.get(featName)

  val containsVariables = features.exists(_._2.containsVariable)

  def substitute(substitution: VariableAssignment): FeatureStruct = {
    val substituted = features.mapValues {
      case v@FVariable(name) => substitution.find(name).getOrElse(v)
      case FList(elems) => FList(elems.map {
        case v@FVariable(name) => substitution.find(name).getOrElse(v).asInstanceOf[FeatureSimpleValue]
        case x => x
      })
      case x => x
    }
    FeatureStruct(substituted)
  }

  override def toString: String =
    if (features.isEmpty) ""
    else features.mkString("[", ",", "]")

}

sealed trait FeatureRhsOperand {
  val containsVariable: Boolean
}

sealed trait FeatureSimpleValue extends FeatureRhsOperand

case object FPlaceholder extends FeatureSimpleValue {
  override val containsVariable: Boolean = false

  override def toString: String = "_"
}

case class FVariable(name: String) extends FeatureSimpleValue {
  override val containsVariable: Boolean = true

  override def toString: String = s"?$name"
}
sealed trait FeatureValue extends FeatureRhsOperand
case class FConst(value: String) extends FeatureValue with FeatureSimpleValue {
  override val containsVariable: Boolean = false

  override def toString: String = value
}
case class FList(elements: List[FeatureSimpleValue]) extends FeatureValue {
  override val containsVariable: Boolean = elements.exists(_.containsVariable)

  override def toString: String = elements.mkString("<", ",", ">")
}

object FeatureStruct {
  val empty = FeatureStruct(Map.empty)
}
