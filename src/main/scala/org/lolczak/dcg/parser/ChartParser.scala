package org.lolczak.dcg.parser

import org.lolczak.dcg.{Production, Grammar, Lexicon, Term}

object ChartParser {

  type State = Set[Edge]

  type Chart = List[State]

  def parseDcg(grammar: Grammar, lexicon: Lexicon, utterance: String): List[ParseTree[String, Term]] = ???

  def scan(word: String, index: Int, lexicon: Lexicon): State = {
    require(index >= 0)
    lexicon.findAllForms(word).map(t => Passive(index, index + 1, t, Node(t, List(Leaf(word)))))
  }

  def predict(grammar: Grammar, edge: Passive): Set[Edge] =
    for {
      Production(lhs, rhs) <- grammar.findProductionsWithHead(edge.found)
    } yield if (rhs.tail.isEmpty) Passive(edge.start, edge.end, lhs, Node(lhs, List(edge.tree))) : Edge
            else Active(edge.start, edge.end, lhs, rhs.tail, List(edge.tree)): Edge

  def combine(chart: Chart, edge: Edge): State = ???

}
