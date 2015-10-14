package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.language.variable.FeatureFunctions._

import scalaz.Scalaz._

package object variable {

  def evalVariableAssignment(production: Production, parsedTerms: List[ParseTree[Term, String]]): Option[VariableAssignment] = {
    val rhs = production.rhs
    val parsedRhs = parsedTerms.map { case Node(term, _, _) => term }
    require(rhs.length == parsedRhs.length)
    val zipped = rhs zip parsedRhs
    val substitutions: List[Option[VariableAssignment]] = zipped map { case (rule, parsed) => findAssignment(rule.fStruct, parsed.fStruct) }
    val maybeSubstitutions: Option[List[VariableAssignment]] = substitutions.sequence[Option, VariableAssignment]
    val maybeSubstitution = maybeSubstitutions.flatMap { list =>
      list.foldLeft[Option[VariableAssignment]](Some(VariableAssignment.empty)) {
        case (None, _) => None
        case (Some(acc), item) => acc union item
      }
    }
    maybeSubstitution
  }

  /**
   * Creates variable assignments based on rule features and features values derived from parsed nodes.
   *
   * @param ruleFeatures
   * @param parsedFeatures
   * @return
   */
  private[variable] def findAssignment(ruleFeatures: FeatureStruct, parsedFeatures: FeatureStruct): Option[VariableAssignment] = {
    val bindings: Set[VariableBinding] = VariableBinding.findVariableBindings(ruleFeatures)
    bindings.foldLeft[Option[VariableAssignment]](Some(VariableAssignment.empty)) {
      case (maybeSubstitution, binding) =>
        for {
          substitution <- maybeSubstitution
          value        <- extractVar(parsedFeatures, binding)
          if !containsVariables(value)
          result       <- substitution.add(binding.varName, value.asInstanceOf[FeatureValue])
        } yield result
    }
  }

  private def extractVar(parsedFeatures: FeatureStruct, binding: VariableBinding): Option[FeatureItem] =
    FeatureZipper.goto(binding.navigation.breadcrumbs)(parsedFeatures)
}
