package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar

import scalaz.\/

trait Completer {

  def complete(chart: Chart)(edge: Passive): Set[Active \/ PassiveCandidate]

  def completeEmpty(grammar: Grammar)(edge: Active): Set[Active \/ PassiveCandidate]

}
