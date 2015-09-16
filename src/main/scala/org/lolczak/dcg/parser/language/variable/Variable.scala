package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{Term, Production}
import org.lolczak.dcg.parser.language.{Node, ParseTree}

import scalaz.Scalaz._

object Variable {

  def unify(production: Production, parsedTerms: List[ParseTree[Term, String]]): Option[Term] = {
    val rhs = production.rhs
    val parsedRhs = parsedTerms.map { case Node(term, _) => term }
    require(rhs.length == parsedRhs.length)
    val zipped = rhs zip parsedRhs
    val substitutions: List[Option[Substitution]] = zipped map { case (rule, parsed) => Substitution.fromFeatures(rule.fStruct, parsed.fStruct) }
    val maybeSubstitutions: Option[List[Substitution]] = substitutions.sequence[Option, Substitution]
    val maybeSubstitution = maybeSubstitutions.flatMap { list =>
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
  }


}
