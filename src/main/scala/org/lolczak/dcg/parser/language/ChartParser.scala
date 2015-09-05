package org.lolczak.dcg.parser.language

import org.lolczak.dcg.parser.language.agreement.FeatureAgreement
import org.lolczak.dcg.{Grammar, Lexicon, Production, Term}

import scala.annotation.tailrec

object ChartParser {

  type Chart = IndexedSeq[State]

  def parseDcg(grammar: Grammar, lexicon: Lexicon, utterance: String, rootSymbol: Option[String] = None): List[ParseTree[Term, String]] = {
    val splittedUtterance = utterance.split(' ').toList
    val finalChart = buildChart(grammar, lexicon, splittedUtterance)
    for {
      Passive(0, end, found, tree) <- finalChart.last.edges.toList
      if found.name == rootSymbol.getOrElse(grammar.start)
    } yield tree
  }

  def buildChart(grammar: Grammar, lexicon: Lexicon, utterance: List[String]): Chart = {
    val indexedUtterance = utterance zip Stream.from(0) toIndexedSeq
    val initialChart: Chart = indexedUtterance map { case (word, idx) => scan(word, idx, lexicon) }
    val f: Chart => Edge => Set[Edge] = (chart: Chart) => {
      case edge: Passive => predict(grammar, edge) ++ combine(chart, edge)
      case _ => Set.empty
    }
    initialChart.foldLeft(IndexedSeq.empty[State]) {
      case (prefix, currentState) => prefix :+ State(generate(f(prefix), currentState.edges))
    }
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
    State(lexicon.findAllForms(word).map(t => Passive(index, index + 1, t, Node(t, List(Leaf(word))))))
  }

  def predict(grammar: Grammar, edge: Passive): Set[Edge] =
    for {
      p@Production(lhs, rhs) <- grammar.findStartingWith(edge.found.name)
      if FeatureAgreement.isConsistent(rhs.head, edge.found)
      newEdge = if (rhs.tail.isEmpty) Passive(edge.start, edge.end, lhs, Node(lhs, List(edge.tree))): Edge
                else Active(edge.start, edge.end, lhs, rhs.tail, List(edge.tree), p): Edge
    } yield newEdge

  def combine(chart: Chart, edge: Passive): Set[Edge] =
    if (edge.start <= 0) Set.empty
    else for {
      Active(start, end, leftTerm, prefix :: rest, parsedPrefix, p) <- chart(edge.start - 1).findActiveStartingWith(edge.found.name)
      if end == edge.start && FeatureAgreement.isConsistent(prefix, edge.found)
      newEdge =  if (rest.isEmpty) Passive(start, edge.end, leftTerm, Node(leftTerm, parsedPrefix :+ edge.tree)): Edge
                 else Active(start, edge.end, leftTerm, rest, parsedPrefix :+ edge.tree, p): Edge
    } yield newEdge

}
