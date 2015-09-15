package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.parser.language.{Node, ParseTree}
import org.lolczak.dcg.{Term, Production, FeatureStruct}

object Variable {

  def unify(production: Production, parsedTerms: List[ParseTree[Term, String]]): Option[Term] = {
    val rhs = production.rhs
    val parsedRhs = parsedTerms.map { case Node(term, _) => term }
    require(rhs.length == parsedRhs.length)
    ???
  }


}
