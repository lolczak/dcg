package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.{FeatureStruct, FeatureValue}

case class VariableAssignment(varName: String, value: FeatureValue)

case class Substitution(assignments: Set[VariableAssignment]) {
  val substitutionMap: Map[String, FeatureValue] = ???
}

object Substitution {

  /**
   * Creates variable assignments based on rule features and features values derived from parsed nodes.
   *
   * @param ruleFeatures
   * @param parsedFeatures
   * @return
   */
  def fromFeatures(ruleFeatures: FeatureStruct, parsedFeatures: FeatureStruct): Option[Substitution] = {
    val bindings = VariableBinding.findVariableBindings(ruleFeatures)
    //todo implement constraint
    /*
    val varNames: Map[String, String] = ruleFeatures.features.filter(_._2.isVariable) map { case (featName, FVariable(varName)) => (varName, featName) }
    val assignments: Map[String, FeatureRhsOperand] = for {
      (varName, featName) <- varNames
      if parsedFeatures.features.contains(featName) && !parsedFeatures.features(featName).isVariable //todo encapsulate it
    } yield (varName, parsedFeatures.features(featName))

    VarAssignments(assignments)
     */
    None
  }

}