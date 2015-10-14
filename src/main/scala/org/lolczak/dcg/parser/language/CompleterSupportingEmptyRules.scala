package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.{Grammar, Production, Term}
import org.lolczak.dcg.parser.language.ToTermOps._

import scalaz.{-\/, \/, \/-}

object CompleterSupportingEmptyRules extends Completer{

  def complete(chart: Chart)(edge: Passive): Set[Active \/ PassiveCandidate] =
    if (edge.start <= 0) Set.empty
    else for {
      Active(start, end, leftTerm, prefix :: rest, parsedPrefix, p) <- chart(edge.start - 1).findActiveStartingWith(edge.found.name)
      if end == edge.start && (prefix matches edge.found)
    } yield tryCreateCombinedEdge(edge, start, leftTerm, rest, parsedPrefix, p)

  def tryCreateCombinedEdge(edge: Passive, start: Int, leftTerm: Term, rest: List[Term], parsedPrefix: List[ParseTree[Term, String]], p: Production): Active \/ PassiveCandidate = {
    if (rest.isEmpty) \/-(PassiveCandidate(start, edge.end, p, parsedPrefix :+ edge.tree))
    else -\/(Active(start, edge.end, leftTerm, rest, parsedPrefix :+ edge.tree, p))
  }

  def completeEmpty(grammar: Grammar)(edge: Active): Set[Active \/ PassiveCandidate] = {
    val found = grammar.nonterminals.emptyTerms.filter(edge.remaining.head matches _)
    found.map { term =>
      if (edge.remaining.tail.nonEmpty)
        -\/(edge.copy(remaining = edge.remaining.tail, parsedPrefix = Node(term, List(Leaf("∅"))) :: edge.parsedPrefix))
      else
        \/-(PassiveCandidate(edge.start, edge.end, edge.production, Node(term, List(Leaf("∅"))) :: edge.parsedPrefix))
    } toSet
  }
}
