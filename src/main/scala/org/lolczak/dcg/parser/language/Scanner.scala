package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar

import scalaz.Reader

trait Scanner {

  def scan(utterance: List[String]): Reader[Grammar, Chart]

}
