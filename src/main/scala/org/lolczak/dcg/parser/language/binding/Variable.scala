package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.parser.language.{Node, ParseTree}
import org.lolczak.dcg.{Term, Production, FeatureStruct}

import scalaz._
import Scalaz._

object Variable {

  def unify(production: Production, parsedTerms: List[ParseTree[Term, String]]): Option[Term] = {
    val rhs = production.rhs
    val parsedRhs = parsedTerms.map { case Node(term, _) => term }
    require(rhs.length == parsedRhs.length)
    val zipped = rhs zip parsedRhs
    val varAss: List[Option[Substitution]] = zipped map { case (rule, parsed) => Substitution.fromFeatures(rule.fStruct, parsed.fStruct) }
    val maybeList: Option[List[Substitution]] = varAss.sequence[Option, Substitution]
    val maybeSubstitution = maybeList.flatMap { list =>
      list.foldLeft[Option[Substitution]](Some(Substitution.empty)) {
        case (None, _) => None
        case (Some(acc), item) => acc union item
      }
    }
    for {
      substitution <- maybeSubstitution
      features = production.lhs.fStruct.substitute(substitution)
      if !features.containsVariables
    } yield Term(production.lhs.name, features)

    /*
    val zipped = rhs zip parsedRhs
      val varAss = zipped map { case (rule, parsed) => fromFeatures(rule.fStruct, parsed.fStruct) }
      val finalAss: Error \/ VarAssignments = varAss.foldLeft[Error \/ VarAssignments](\/-(empty)) {
        case (prevResult, item) => prevResult.flatMap(prev => prev.combine(item))
      }
      //todo loging add to validator
      val edge = for {
        assignment <- finalAss
        bindedFeatures <- substitute(production.lhs.fStruct, assignment)
        term = Term(production.lhs.name, bindedFeatures)
        tree = Node(term, parsedTerms)
      } yield Passive(start, end, term, tree)
     */

  }


}
