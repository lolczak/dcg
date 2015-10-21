package org.lolczak.dcg.model

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

case class FExpr(exprCode: String) extends FeatureItem {

  override def toString: String = "{" + exprCode + "}"

}

object FeatureStruct {
  val empty = FeatureStruct(Map.empty)
}