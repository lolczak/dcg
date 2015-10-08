package org.lolczak.dcg.model

import org.lolczak.dcg.parser.language.variable.VariableAssignment

sealed trait FeatureItem {
  val containsVariable: Boolean
}

sealed trait FeatureValue extends FeatureItem

case class FeatureStruct(features: Map[String, FeatureItem]) extends FeatureValue {

  val containsVariable = features.exists(_._2.containsVariable)

  def apply(featName: String): Option[FeatureItem] = features.get(featName)

  override def toString: String =
    if (features.isEmpty) ""
    else features.mkString("[", ",", "]")

}

sealed trait FeatureSimpleValue extends FeatureItem

case object FPlaceholder extends FeatureSimpleValue {
  override val containsVariable: Boolean = false

  override def toString: String = "_"
}

case class FVariable(name: String) extends FeatureSimpleValue {
  override val containsVariable: Boolean = true

  override def toString: String = s"?$name"
}

case class FConst(value: String) extends FeatureValue with FeatureSimpleValue {
  override val containsVariable: Boolean = false

  override def toString: String = value
}
case class FList(elements: List[FeatureItem]) extends FeatureValue {
  override val containsVariable: Boolean = elements.exists(_.containsVariable)

  override def toString: String = elements.mkString("<", ",", ">")
}

object FeatureStruct {
  val empty = FeatureStruct(Map.empty)
}

object FeatureItem {

  def findVariables(root: FeatureItem): List[FeatureZipper] =
    FeatureZipper.filter {
      case _: FVariable => true
      case _ => false
    }(root)

  def containsVariables(root: FeatureItem): Boolean = findVariables(root).nonEmpty

  def substitute(root: FeatureItem, substitution: VariableAssignment): FeatureItem = {
    findVariables(root).map(_.breadcrumbs).foldLeft(root) {
      case (item, path) => FeatureZipper.alter { case v@FVariable(name) => substitution.find(name).getOrElse(item) } (path) (item)
    }
  }

}
