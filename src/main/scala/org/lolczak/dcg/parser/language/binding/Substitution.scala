package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.{FeatureStruct, FeatureValue}

case class Substitution(private val assignments: Map[String, FeatureValue]) {

  val variables = assignments.keySet

  def add(varName: String, value: FeatureValue): Option[Substitution] =
    if (assignments.contains(varName) && assignments(varName) != value) None
    else Some(Substitution(assignments + (varName -> value)))

  def find(varName: String): Option[FeatureValue] = assignments.get(varName)

  def union(that: Substitution): Option[Substitution] = {
    val consistent = assignments.forall { case (varName, value) => that.find(varName).map(_ == value).getOrElse(true) }
    if (consistent) combine(that)
    else None
  }

  private def combine(that: Substitution): Some[Substitution] = {
    val allVariables = this.variables ++ that.variables
    val allAssignments = for {
      varName <- allVariables
      value = this.find(varName).orElse(that.find(varName)).get
    } yield (varName, value)
    Some(Substitution(allAssignments.toMap))
  }
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