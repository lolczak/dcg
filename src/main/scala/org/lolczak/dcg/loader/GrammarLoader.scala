package org.lolczak.dcg.loader

import org.lolczak.dcg.model.Grammar

import scalaz.\/

object GrammarLoader {

  def load(grammarTxt: String): GrammarFailure \/ Grammar = ???
  //      lexicon      = Lexicon.fromProductions(terminals: _*)
}

sealed trait GrammarFailure