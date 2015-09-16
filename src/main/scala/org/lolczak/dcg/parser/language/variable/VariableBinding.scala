package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.{FVariable, FeatureStruct}

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