package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.{Grammar, Production, Term}

import scalaz.{-\/, \/, \/-}

object PredictorSupportingEmptyRules extends Predictor {

  override def predict(grammar: Grammar)(edge: Passive): Set[Active \/ PassiveCandidate] = {
    for {
      (production, prefix) <- grammar.nonterminals.findPrefix(edge.found.name)
      focus = production.rhs(prefix.size)
      if focus matches edge.found
    } yield tryCreatePredictedEdge(edge, production, prefix)
  }

  def tryCreatePredictedEdge(edge: Passive, p: Production, prefix: List[(Term, Term)]): Active \/ PassiveCandidate = {
    val tail = p.rhs.drop(prefix.size + 1)
    val parsedTerms = prefix.map(x => Node(x._2, List(Leaf("∅"))))
    if (tail.isEmpty) \/-(PassiveCandidate(edge.start, edge.end, p, parsedTerms ++ List(edge.tree)))
    else -\/(Active(edge.start, edge.end, p.lhs, tail, parsedTerms ++ List(edge.tree), p))
  }

}
