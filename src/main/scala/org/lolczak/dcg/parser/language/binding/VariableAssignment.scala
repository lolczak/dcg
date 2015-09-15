package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.{FeatureStruct, FeatureValue}

case class Substitution(private val assignments: Map[String, FeatureValue]) {

  def add(varName: String, value: FeatureValue): Option[Substitution] =
    if (assignments.contains(varName) && assignments(varName) != value) None
    else Some(Substitution(assignments + (varName -> value)))

  def find(varName: String): Option[FeatureValue] = assignments.get(varName)

  def union(that: Substitution): Option[Substitution] = ???

}

object Substitution {

  val empty = Substitution()

  def apply(elems: (String, FeatureValue)*): Substitution = Substitution(Map(elems: _*))

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
          result <- substitution.add(binding.varName, value.asInstanceOf[FeatureValue])
        } yield result
    }
  }

}