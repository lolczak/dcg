package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.{FeatureStruct, FVariable}

case class VariableBinding(varName: String, featureName: String)

object VariableBinding {

  /**
   * Finds all variable bindings in productions features.
   *
   * @param ruleFeatures
   * @return
   */
  def findVariableBindings(ruleFeatures: FeatureStruct): Set[VariableBinding] =
    ruleFeatures.features.filter(_._2.isVariable) map {
      case (featName, FVariable(varName)) => VariableBinding(varName, featName)
    } toSet

}