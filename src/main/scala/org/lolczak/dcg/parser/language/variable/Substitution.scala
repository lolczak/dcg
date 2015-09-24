package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FeatureStruct, FeatureValue, Production, Term}
import org.lolczak.dcg.parser.language.guard.{EvalResult, GuardEval, GroovyGuardEval}
import org.lolczak.dcg.parser.language.{Node, ParseTree}

import scalaz.{\/-, -\/}
import scalaz.Scalaz._

object Substitution {

  def substitute(production: Production, parsedTerms: List[ParseTree[Term, String]], guardEval: GuardEval): Option[Term] = {
    val rhs = production.rhs
    val parsedRhs = parsedTerms.map { case Node(term, _) => term }
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
    for {
      substitution <- maybeSubstitution.flatMap(eval(production, guardEval))
      features = production.lhs.fStruct.substitute(substitution)
      if !features.containsVariables
    } yield Term(production.lhs.name, features)
  }

  private def eval(production: Production, guardEval: GuardEval)(unifiedAssignment: VariableAssignment): Option[VariableAssignment] = {
    if (production.containsGuard) {
      val result = guardEval.eval(production.maybeSnippet.get, unifiedAssignment)
      result match {
        case -\/(err) => throw new RuntimeException(err.toString)
        case \/-(EvalResult(_, false)) => None
        case \/-(EvalResult(variableAssignment, true))=> Some(variableAssignment)
      }
    } else Some(unifiedAssignment)

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
          value <- parsedFeatures(binding.featureName)
          if !value.isVariable
          result <- substitution.add(binding.varName, value.asInstanceOf[FeatureValue])
        } yield result
    }
  }


}
