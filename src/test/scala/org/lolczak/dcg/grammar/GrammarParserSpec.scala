package org.lolczak.dcg.grammar

import org.scalatest.{Matchers, WordSpec}

class GrammarParserSpec extends WordSpec with Matchers {

  "Term parser" should {
    "parse simple term containing only symbol" in {
      //given
      val termString = "Verb"
      //when
      val result: GrammarParser.ParseResult[String] = GrammarParser.term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern {case GrammarParser.Success("Verb", _) => }
    }
  }

}
