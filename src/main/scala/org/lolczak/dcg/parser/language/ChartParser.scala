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

  private val complete = CompleterSupportingEmptyRules.complete(_)

  private val completeEmpty = CompleterSupportingEmptyRules.completeEmpty(grammar)(_)

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

  def createPassive(start: Int, end: Int, production: Production, parsedTerms: List[ParseTree[Term, String]]): Option[Passive] =
    for {
      term <- Substitution.substitute(production, parsedTerms, guardEval) //todo maybe Reader is better option
      tree = Node(term, parsedTerms, production.id)
    } yield Passive(start, end, term, tree)

}
