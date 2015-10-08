package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FeatureZipper, FeatureItem, FVariable, FeatureStruct}

case class VariableBinding(varName: String, navigation: FeatureZipper)

object VariableBinding {

  /**
   * Finds all variable bindings in productions features.
   *
   * @param ruleFeatures
   * @return
   */
  def findVariableBindings(ruleFeatures: FeatureStruct): Set[VariableBinding] =
    FeatureItem.findVariables(ruleFeatures) map {
      case z@FeatureZipper(FVariable(name), path) => VariableBinding(name, z)
    } toSet

}