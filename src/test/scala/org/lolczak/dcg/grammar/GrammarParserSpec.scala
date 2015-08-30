package org.lolczak.dcg.grammar

import org.lolczak.dcg.{Term, FeatureStruct}
import org.lolczak.dcg.grammar.GrammarParser._
import org.scalatest.{Matchers, WordSpec}

class GrammarParserSpec extends WordSpec with Matchers {

  "Term parser" should {
    "parse simple term containing only symbol" in {
      //given
      val termString = "Verb"
      //when
      val result: ParseResult[Term] = term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern {case Success(Term("Verb", FeatureStruct.empty), _) => }
    }

  }

}
