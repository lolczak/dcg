package org.lolczak.dcg.parser

import org.lolczak.dcg.{Grammar, Lexicon, Production, Term}

import scala.annotation.tailrec

object ChartParser {

  type State = Set[Edge] //todo refactor

  type Chart = IndexedSeq[State]

  def parseDcg(grammar: Grammar, lexicon: Lexicon, utterance: String): List[ParseTree[Term, String]] = {
    val splittedUtterance = utterance.split(' ').toList
    val finalChart = buildChart(grammar, lexicon, splittedUtterance)
    for {
      Passive(0, end, found, tree) <- finalChart.last.toList
      if found.name == grammar.start
    } yield tree
  }

  def buildChart(grammar: Grammar, lexicon: Lexicon, utterance: List[String]): Chart = {
    val indexedUtterance = utterance zip Stream.from(0) toIndexedSeq
    val initialChart: Chart = indexedUtterance map { case (word, idx) => scan(word, idx, lexicon) }
    val f: Chart => Edge => Set[Edge] = (chart: Chart) => {
      case edge: Passive =>
        predict(grammar, edge) ++ combine(chart, edge)
      case _ => Set.empty
    }
    val finalChart = initialChart.foldLeft(IndexedSeq.empty[State]) {
      case (prefix, currentState) =>
        val generated = generate(f(prefix), currentState)
        prefix :+ generated
    }
    finalChart
  }

  def generate[A](f: A => Set[A], source: Set[A]): Set[A] = {
    @tailrec
    def recGen(oldSet: Set[A], newSet: Set[A]): Set[A] = {
      val generated = newSet.flatMap(f)
      if (generated.isEmpty) oldSet
      else recGen(generated ++ oldSet, generated -- oldSet)
    }
    recGen(source, source)
  }

  def scan(word: String, index: Int, lexicon: Lexicon): State = {
    require(index >= 0)
    lexicon.findAllForms(word).map(t => Passive(index, index + 1, t, Node(t, List(Leaf(word)))))
  }

  def predict(grammar: Grammar, edge: Passive): Set[Edge] =
    for {
      Production(lhs, rhs) <- grammar.findProductionsWithHead(edge.found)
    } yield if (rhs.tail.isEmpty) Passive(edge.start, edge.end, lhs, Node(lhs, List(edge.tree))): Edge
    else Active(edge.start, edge.end, lhs, rhs.tail, List(edge.tree)): Edge

  def combine(chart: Chart, edge: Passive): Set[Edge] =
    if (edge.start <= 0) Set.empty
    else for {
      Active(start, end, leftTerm, prefix :: rest, parsedPrefix) <- chart(edge.start - 1) //todo find edges with prefix
      if prefix.name == edge.found.name && end == edge.start
    } yield if (rest.isEmpty) Passive(start, edge.end, leftTerm, Node(leftTerm, parsedPrefix :+ edge.tree)): Edge
      else Active(start, edge.end, leftTerm, rest, parsedPrefix :+ edge.tree): Edge

}
