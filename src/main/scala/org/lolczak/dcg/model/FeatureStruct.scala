package org.lolczak.dcg.model

import org.lolczak.dcg.parser.language.variable.VariableAssignment

sealed trait FeatureItem

sealed trait FeatureValue extends FeatureItem

case class FeatureStruct(features: Map[String, FeatureItem]) extends FeatureValue {

  def apply(featName: String): Option[FeatureItem] = features.get(featName)

  override def toString: String =
    if (features.isEmpty) ""
    else features.mkString("[", ",", "]")

}

case object FPlaceholder extends FeatureItem {

  override def toString: String = "_"

}

case class FVariable(name: String) extends FeatureItem {

  override def toString: String = s"?$name"

}

case class FConst(value: String) extends FeatureValue {

  override def toString: String = value

}
case class FList(elements: List[FeatureItem]) extends FeatureValue {

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
