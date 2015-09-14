package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.{FeatureStruct, FeatureValue}

case class VariableAssignment(varName: String, value: FeatureValue)

case class Substitution(assignments: Set[VariableAssignment]) {
  val substitutionMap: Map[String, FeatureValue] = assignments.map(x=> (x.varName, x.value)).toMap

  def add(another: VariableAssignment): Option[Substitution] = {
    if(substitutionMap.contains(another.varName) && substitutionMap(another.varName) != another.value) None
    else Some(Substitution(assignments + another))
  }
}

object Substitution {

  val empty:Substitution = Substitution(Set.empty)

  /**
   * Creates variable assignments based on rule features and features values derived from parsed nodes.
   *
   * @param ruleFeatures
   * @param parsedFeatures
   * @return
   */
  def fromFeatures(ruleFeatures: FeatureStruct, parsedFeatures: FeatureStruct): Option[Substitution] = {
    val bindings: Set[VariableBinding] = VariableBinding.findVariableBindings(ruleFeatures)

    bindings.foldLeft[Option[Substitution]](Some(Substitution.empty)) {
      case (maybeSubstitution, binding) =>
        for {
          substitution <- maybeSubstitution
          value <- parsedFeatures(binding.featureName)
          if !value.isVariable
          result <- substitution.add(VariableAssignment(binding.varName, value.asInstanceOf[FeatureValue]))
        } yield result
    }
  }

}