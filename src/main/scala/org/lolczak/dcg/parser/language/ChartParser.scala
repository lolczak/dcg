package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.language.guard.{GroovyGuardEval, GuardEval}
import org.lolczak.dcg.parser.language.variable.Substitution

import scala.annotation.tailrec

object ChartParser {

  type Chart = IndexedSeq[State]

  def parseDcg(grammar: Grammar, utterance: String, rootSymbol: Option[String] = None, guardEval: GuardEval = new GroovyGuardEval): List[ParseTree[Term, String]] = {
    val splitUtterance = utterance.split(' ').toList
    val finalChart = buildChart(grammar, splitUtterance, guardEval)
    for {
      Passive(0, end, found, tree) <- finalChart.last.edges.toList
      if found.name == rootSymbol.getOrElse(grammar.nonterminals.start)
    } yield tree
  }

  def buildChart(grammar: Grammar, utterance: List[String], guardEval: GuardEval): Chart = {
    val indexedUtterance = utterance zip Stream.from(0) toIndexedSeq
    val initialChart: Chart = indexedUtterance map { case (word, idx) => scan(word, idx, grammar.lexicon) }
    val f: Chart => Edge => Set[Edge] = (chart: Chart) => {
      case edge: Passive => predict(grammar.nonterminals, edge, guardEval) ++ combine(chart, edge, guardEval)
      case _ => Set.empty
    }
    initialChart.foldLeft(IndexedSeq.empty[State]) {
      case (prefix, currentState) => prefix :+ State(generate(f(prefix), currentState.edges))
    }
  }

  //todo refactor, generic version in utils
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

  def predict(grammar: Nonterminals, edge: Passive, guardEval: GuardEval): Set[Edge] =
    for {
      p@Production(lhs, rhs, snippet) <- grammar.findStartingWith(edge.found.name)
      maybeNewEdge = tryCreatePredictedEdge(edge, p, lhs, rhs, guardEval)
      if FeatureAgreement.isConsistent(rhs.head, edge.found) && maybeNewEdge.isDefined
    } yield maybeNewEdge.get

  def tryCreatePredictedEdge(edge: Passive, p: Production, lhs: Term, rhs: List[Term], guardEval: GuardEval): Option[Edge] = {
    if (rhs.tail.isEmpty) createPassive(edge.start, edge.end, p, List(edge.tree), guardEval)
    else Some(Active(edge.start, edge.end, lhs, rhs.tail, List(edge.tree), p))
  }

  def combine(chart: Chart, edge: Passive, guardEval: GuardEval): Set[Edge] =
    if (edge.start <= 0) Set.empty
    else for {
      Active(start, end, leftTerm, prefix :: rest, parsedPrefix, p) <- chart(edge.start - 1).findActiveStartingWith(edge.found.name)
      maybeNewEdge = tryCreateCombinedEdge(edge, start, leftTerm, rest, parsedPrefix, p, guardEval)
      if end == edge.start && FeatureAgreement.isConsistent(prefix, edge.found) && maybeNewEdge.isDefined
    } yield maybeNewEdge.get

  def tryCreateCombinedEdge(edge: Passive, start: Int, leftTerm: Term, rest: List[Term], parsedPrefix: List[ParseTree[Term, String]], p: Production, guardEval: GuardEval): Option[Edge] = {
    if (rest.isEmpty) createPassive(start, edge.end, p, parsedPrefix :+ edge.tree, guardEval)
    else Some(Active(start, edge.end, leftTerm, rest, parsedPrefix :+ edge.tree, p))
  }

  def createPassive(start: Int, end: Int, production: Production, parsedTerms: List[ParseTree[Term, String]], guardEval: GuardEval): Option[Passive] =
    for {
      term <- Substitution.substitute(production, parsedTerms, guardEval) //todo maybe Reader is better option
      tree = Node(term, parsedTerms)
    } yield Passive(start, end, term, tree)

}
