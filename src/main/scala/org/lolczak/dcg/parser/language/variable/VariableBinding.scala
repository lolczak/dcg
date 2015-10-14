package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FeatureZipper, FVariable, FeatureStruct}
import FeatureFunctions._

case class VariableBinding(varName: String, navigation: FeatureZipper)

object VariableBinding {

  /**
   * Finds all variable bindings in productions features.
   *
   * @param ruleFeatures
   * @return
   */
  def findVariableBindings(ruleFeatures: FeatureStruct): Set[VariableBinding] =
    findVariables(ruleFeatures) map {
      case z@FeatureZipper(FVariable(name), path) => VariableBinding(name, z)
    } toSet

}