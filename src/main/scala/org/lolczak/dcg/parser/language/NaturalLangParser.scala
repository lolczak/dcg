package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Term

trait NaturalLangParser {

  def parse(utterance: String): List[ParseTree[Term, String]]

}
