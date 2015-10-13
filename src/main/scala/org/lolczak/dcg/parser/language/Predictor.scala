package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar

import scalaz.{\/, Reader}

trait Predictor {

  def predict(edge: Passive): Reader[Grammar, Set[Active \/ PassiveCandidate]]

}
