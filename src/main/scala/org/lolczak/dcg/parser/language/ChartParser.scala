package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.language.guard.{GroovyGuardEval, GuardEval}
import org.lolczak.dcg.parser.language.variable.FeatureFunctions
import org.lolczak.dcg.parser.language.variable.FeatureFunctions._
import org.lolczak.util.Generators._

import scalaz.{-\/, \/, \/-}

class ChartParser(grammar: Grammar, rootSymbol: Option[String] = None) extends NaturalLangParser {

//  def this(grammar: Grammar) = this(grammar)

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

  private def buildChart(utterance: List[String]): Chart = {
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
    case \/-(p: PassiveCandidate) => tryCreatePassive(p).map(Set[Edge](_)).getOrElse(Set.empty)
  }

  private def tryCreatePassive(candidate: PassiveCandidate): Option[Passive] =
    for {
      unifiedAssignment <- variable.evalVariableAssignment(candidate.production, candidate.parsedTerms)
      finalAssignment   <- guard.evalGuard(candidate.production)(unifiedAssignment)
      features = FeatureFunctions.substitute(candidate.production.lhs.fStruct, finalAssignment)
      if !containsVariables(features)
      term = Term(candidate.production.lhs.name, features.asInstanceOf[FeatureStruct])
      tree = Node(term, candidate.parsedTerms, candidate.production.id)
    } yield Passive(candidate.start, candidate.end, term, tree)

}
