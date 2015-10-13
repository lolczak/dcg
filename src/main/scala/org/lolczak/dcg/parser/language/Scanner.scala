package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar

trait Scanner {

  def scan(grammar: Grammar)(utterance: List[String]): Chart

}

object SimpleScanner extends Scanner {

  override def scan(grammar: Grammar)(utterance: List[String]): Chart = {
    val indexedUtterance = utterance.zipWithIndex
    indexedUtterance map { case (word, idx) =>
      State(grammar.lexicon.findAllForms(word).map(t => Passive(idx, idx + 1, t, Node(t, List(Leaf(word))))))
    } toIndexedSeq
  }

}