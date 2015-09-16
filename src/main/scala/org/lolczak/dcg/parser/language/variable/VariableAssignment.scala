package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FeatureValue, FeatureStruct}

case class VariableAssignment(private val assignments: Map[String, FeatureValue]) {

  val variables = assignments.keySet

  def add(varName: String, value: FeatureValue): Option[VariableAssignment] =
    if (assignments.contains(varName) && assignments(varName) != value) None
    else Some(VariableAssignment(assignments + (varName -> value)))

  def find(varName: String): Option[FeatureValue] = assignments.get(varName)

  def union(that: VariableAssignment): Option[VariableAssignment] = {
    val consistent = assignments.forall { case (varName, value) => that.find(varName).map(_ == value).getOrElse(true) }
    if (consistent) combine(that)
    else None
  }

  private def combine(that: VariableAssignment): Some[VariableAssignment] = {
    val allVariables = this.variables ++ that.variables
    val allAssignments = for {
      varName <- allVariables
      value = this.find(varName).orElse(that.find(varName)).get
    } yield (varName, value)
    Some(VariableAssignment(allAssignments.toMap))
  }
}

object VariableAssignment {

  val empty = VariableAssignment()

  def apply(elems: (String, FeatureValue)*): VariableAssignment = VariableAssignment(Map(elems: _*))

  /**
   * Creates variable assignments based on rule features and features values derived from parsed nodes.
   *
   * @param ruleFeatures
   * @param parsedFeatures
   * @return
   */
  def fromFeatures(ruleFeatures: FeatureStruct, parsedFeatures: FeatureStruct): Option[VariableAssignment] = {
    val bindings: Set[VariableBinding] = VariableBinding.findVariableBindings(ruleFeatures)
    bindings.foldLeft[Option[VariableAssignment]](Some(VariableAssignment.empty)) {
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