package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FList, FVariable, FeatureStruct}

case class VariableBinding(varName: String, featureName: String, maybeIndex: Option[Int] = None)

object VariableBinding {

  /**
   * Finds all variable bindings in productions features.
   *
   * @param ruleFeatures
   * @return
   */
  def findVariableBindings(ruleFeatures: FeatureStruct): Set[VariableBinding] =
    ruleFeatures.features.filter(_._2.isVariable) flatMap {
      case (featName, FVariable(varName)) => Iterable(VariableBinding(varName, featName))
      case (featName, FList(elements)) =>
        elements.zipWithIndex.filter(_._1.isVariable).map{case (FVariable(varName), index) => VariableBinding(varName, featName, Some(index))}
    } toSet

}