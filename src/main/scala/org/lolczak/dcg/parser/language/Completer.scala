package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar

import scalaz.\/

trait Completer {

  def combine(chart: Chart)(edge: Passive): Set[Active \/ PassiveCandidate]

  def combineEmpty(grammar: Grammar)(edge: Active): Set[Active \/ PassiveCandidate]

}
