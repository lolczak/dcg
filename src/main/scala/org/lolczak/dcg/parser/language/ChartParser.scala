package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.language.guard.{GroovyGuardEval, GuardEval}
import org.lolczak.dcg.parser.language.variable.Substitution
import org.lolczak.util.Generators._

import scalaz.{\/, \/-, -\/}

class ChartParser(grammar: Grammar, guardEval: GuardEval, rootSymbol: Option[String] = None) extends NaturalLangParser {

  def this(grammar: Grammar) = this(grammar, new GroovyGuardEval(grammar.importDirectives.map(_.file)), None)

  private val scan = SimpleScanner.scan(grammar)(_)

  private val predict = PredictorSupportingEmptyRules.predict(grammar)(_)

  private val complete = CompleterSupportingEmptyRules.combine(_)

  private val completeEmpty = CompleterSupportingEmptyRules.combineEmpty(grammar)(_)

  def parse(utterance: String): List[ParseTree[Term, String]] = {
    val splitUtterance = utterance.split(' ').toList
    val finalChart = buildChart(splitUtterance)
    for {
      Passive(0, end, found, tree) <- finalChart.last.edges.toList
      if found.name == rootSymbol.getOrElse(grammar.nonterminals.start)
    } yield tree
  }

  def buildChart(utterance: List[String]): Chart = {
    val initialChart: Chart = scan(utterance)
    val f: Chart => Edge => Set[Edge] = (chart: Chart) => {
      case edge: Passive => predict(edge).flatMap(processCandidates) ++ complete(chart)(edge).flatMap(processCandidates)
      case edge: Active  => completeEmpty(edge).flatMap(processCandidates)
    }
    initialChart.foldLeft(IndexedSeq.empty[State]) {
      case (prefix, currentState) => prefix :+ State(generate(f(prefix), currentState.edges))
    }
  }

  private val processCandidates: Active \/ PassiveCandidate => Set[Edge] = {
    case -\/(a: Active)           => Set(a)
    case \/-(p: PassiveCandidate) => createPassive(p.start, p.end, p.production, p.parsedTerms).map(Set[Edge](_)).getOrElse(Set.empty)
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
      tree = Node(term, parsedTerms, production.id)
    } yield Passive(start, end, term, tree)

}
