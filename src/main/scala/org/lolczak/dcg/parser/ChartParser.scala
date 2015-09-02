package org.lolczak.dcg.parser

import org.lolczak.dcg.{Grammar, Lexicon, Term}

object ChartParser {

  type State = Set[Edge]

  type Chart = List[State]

  def parseDcg(grammar: Grammar, lexicon: Lexicon, utterance: String): List[ParseTree[String, Term]] = ???

  def scan(word: String, index: Int, lexicon: Lexicon): State =
    lexicon.findAllForms(word).map(t => Passive(index, index+1, t, Node(t, List(Leaf(word)))))

  def predict(grammar: Grammar, edge: Edge): State = ???

  def combine(chart: Chart, edge: Edge): State = ???

}
