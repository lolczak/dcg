package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.language.guard.{GroovyGuardEval, GuardEval}
import org.lolczak.dcg.parser.language.variable.Substitution

import scala.annotation.tailrec

class ChartParser(grammar: Grammar, guardEval: GuardEval, rootSymbol: Option[String] = None) extends NaturalLangParser {

  def this(grammar: Grammar) = this(grammar, new GroovyGuardEval(grammar.importDirectives.map(_.file)), None)

  type Chart = IndexedSeq[State]

  def parse(utterance: String): List[ParseTree[Term, String]] = {
    val splitUtterance = utterance.split(' ').toList
    val finalChart = buildChart(splitUtterance)
    for {
      Passive(0, end, found, tree) <- finalChart.last.edges.toList
      if found.name == rootSymbol.getOrElse(grammar.nonterminals.start)
    } yield tree
  }

  def buildChart(utterance: List[String]): Chart = {
    val indexedUtterance = utterance zip Stream.from(0) toIndexedSeq
    val initialChart: Chart = indexedUtterance map { case (word, idx) => scan(word, idx, grammar.lexicon) }
    val f: Chart => Edge => Set[Edge] = (chart: Chart) => {
      case edge: Passive => predict(grammar.nonterminals, edge) ++ combine(chart, edge)
      case edge: Active  => combineEmpty(edge)
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

  //todo refactor, extract, use reader, remove nonterminals
  def predict(nonterminals: Nonterminals, edge: Passive): Set[Edge] =
    for {
      (p@Production(lhs, rhs, snippet, id), prefix) <- nonterminals.findPrefix(edge.found.name)
      maybeNewEdge = tryCreatePredictedEdge(edge, p, prefix)
      focus = p.rhs(prefix.size)
      if (focus matches edge.found) && maybeNewEdge.isDefined
    } yield maybeNewEdge.get

  def tryCreatePredictedEdge(edge: Passive, p: Production, prefix: List[(Term,Term)]): Option[Edge] = {
    val focus = p.rhs(prefix.size)
    val tail = p.rhs.drop(prefix.size+1)
    val parsedTerms = prefix.map(x => Node(x._2, List(Leaf("∅"))))
    if (tail.isEmpty) createPassive(edge.start, edge.end, p, parsedTerms ++ List(edge.tree))
    else Some(Active(edge.start, edge.end, p.lhs, tail, parsedTerms ++ List(edge.tree), p))
  }

  def combine(chart: Chart, edge: Passive): Set[Edge] =
    if (edge.start <= 0) Set.empty
    else for {
      Active(start, end, leftTerm, prefix :: rest, parsedPrefix, p) <- chart(edge.start - 1).findActiveStartingWith(edge.found.name)
      maybeNewEdge = tryCreateCombinedEdge(edge, start, leftTerm, rest, parsedPrefix, p)
      if end == edge.start && FeatureAgreement.isConsistent(prefix, edge.found) && maybeNewEdge.isDefined
    } yield maybeNewEdge.get

  def tryCreateCombinedEdge(edge: Passive, start: Int, leftTerm: Term, rest: List[Term], parsedPrefix: List[ParseTree[Term, String]], p: Production): Option[Edge] = {
    if (rest.isEmpty) createPassive(start, edge.end, p, parsedPrefix :+ edge.tree)
    else Some(Active(start, edge.end, leftTerm, rest, parsedPrefix :+ edge.tree, p))
  }

  def combineEmpty(edge: Active): Set[Edge] = {
    val found = grammar.nonterminals.emptyTerms.filter(edge.remaining.head matches _)
    found.map { term =>
      if (edge.remaining.tail.nonEmpty)
        Some(edge.copy(remaining = edge.remaining.tail, parsedPrefix = Node(term, List(Leaf("∅"))) :: edge.parsedPrefix))
      else
        createPassive(edge.start, edge.end, edge.production, Node(term, List(Leaf("∅"))) :: edge.parsedPrefix)
    } filter (_.isDefined) map (_.get) toSet
  }

  def createPassive(start: Int, end: Int, production: Production, parsedTerms: List[ParseTree[Term, String]]): Option[Passive] =
    for {
      term <- Substitution.substitute(production, parsedTerms, guardEval) //todo maybe Reader is better option
      tree = Node(term, parsedTerms)
    } yield Passive(start, end, term, tree)

}
