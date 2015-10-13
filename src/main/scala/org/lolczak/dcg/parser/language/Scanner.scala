package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar
import org.lolczak.dcg.parser.language.State

import scalaz._

trait Scanner {

  def scan(utterance: List[String]): Reader[Grammar, Chart]

}

object SimpleScanner extends Scanner {

  override def scan(utterance: List[String]): Reader[Grammar, Chart] =
    Reader { grammar =>
      val indexedUtterance = utterance.zipWithIndex.toIndexedSeq
      indexedUtterance map {
        case (word, idx) =>
          State(grammar.lexicon.findAllForms(word).map(t => Passive(idx, idx + 1, t, Node(t, List(Leaf(word))))))
      }
    }

}