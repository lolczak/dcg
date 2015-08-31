package org.lolczak.dcg.grammar

import org.lolczak.dcg.grammar.GrammarParser._
import org.lolczak.dcg._
import org.scalatest.{Matchers, WordSpec}
import org.lolczak.dcg.Grammar._

class GrammarParserSpec extends WordSpec with Matchers {

  "Term parser" should {
    "parse simple term containing only symbol" in {
      //given
      val termString = "Verb"
      //when
      val result = term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern { case Success(Term("Verb", FeatureStruct.empty), _) => }
    }

    "parse term containing feature" in {
      //given
      val termString = "Verb[Per=frst]"
      val ExpectedFStruct = FeatureStruct(Map("Per" -> FConst("frst")))
      //when
      val result = term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern { case Success(Term("Verb", ExpectedFStruct), _) => }
    }

    "parse term containing features" in {
      //given
      val termString = "Verb[Per=frst, Num=?n, Cas=Acc]"
      val ExpectedFStruct = FeatureStruct(Map("Per" -> FConst("frst"), "Num" -> FVariable("n"), "Cas" -> FConst("Acc")))
      //when
      val result = term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern { case Success(Term("Verb", ExpectedFStruct), _) => }
    }

  }

  "Production parser" should {
    "parser simple nonterminal production" in {
      //given
      val nonterminalString = "S -> NP VP "
      val ExpectedProduction = "S" ~> ("NP", "VP")
      //when
      val result = nonterminal(new GrammarParser.lexical.Scanner(nonterminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }
  }

}
