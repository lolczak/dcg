package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar

import scalaz.\/

trait Predictor {

  def predict(grammar: Grammar)(edge: Passive): Set[Active \/ PassiveCandidate]

}
