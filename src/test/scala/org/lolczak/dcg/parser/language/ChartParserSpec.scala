package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import Grammar._
import org.lolczak.dcg.parser.TestData2
import org.lolczak.dcg.parser.grammar.GrammarParser
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _}
import org.lolczak.dcg.parser.language.guard.GroovyGuardEval
import org.scalatest.{Matchers, WordSpec}

import scala.Predef.{augmentString => _, wrapString => _, _}

class ChartParserSpec extends WordSpec with Matchers {

  "A parser" should {

    "match rhs terms with matching features" in {
      //given
      val nonterminals = Nonterminals("S",
        List(
          "NP"("Num" -> FConst("pl")) ~>("Det"("Num" -> FConst("pl")), "Noun"("Num" -> FConst("pl"))),
          "NP"("Num" -> FConst("sg")) ~>("Det"("Num" -> FConst("sg")), "Noun"("Num" -> FConst("sg"))),
          "PP" ~>("Prep", "NP")
        )
      )
      //when
      val objectUnderTest = new ChartParser(Grammar(nonterminals, TestData2.lexicon),new GroovyGuardEval, Some("NP"))
      val result = objectUnderTest.parse("these planes")
      //then
      val ExpectedFeatures = FeatureStruct(Map("Num" -> FConst("pl")))
      result should have size 1
      result.head should matchPattern {
        case Node(Term("NP", ExpectedFeatures), _, _) =>
      }
    }

    "bind variables when they are consistent" in {
      //given
      val nonterminals = Nonterminals("NP",
        List(
          "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n"))),
          "PP" ~>("Prep", "NP")
        )
      )
      //when
      val objectUnderTest = new ChartParser(Grammar(nonterminals, TestData2.lexicon),new GroovyGuardEval, Some("NP"))
      val result = objectUnderTest.parse("these planes")
      //then
      val ExpectedFeatures = FeatureStruct(Map("Num" -> FConst("pl")))
      result should have size 1
      result.head should matchPattern {
        case Node(Term("NP", ExpectedFeatures), _, _) =>
      }
    }

    "find parse tree for correct utterance" in {
      //when
      val objectUnderTest = new ChartParser(TestData2.grammar)
      val result = objectUnderTest.parse(TestData2.utterance)
      //then
      result should have size 1
      result.head should matchPattern {
        case Node(Term("S", _), List(Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf(these)), _), Node(Term("Noun", _), List(Leaf("planes")), _)), _), Node(Term("VP", _), List(Node(Term("VP", _), List(Node(Term("Verb", _), List(Leaf(fly)), _)), _), Node(Term("PP", _), List(Node(Term("Prep", _), List(Leaf("like")), _), Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf("an")), _), Node(Term("Noun", _), List(Leaf("arrow")), _)), _)), _)), _)), _) =>
      }
    }

    "return empty list for incorrrect utterance" in {
      //when
      val objectUnderTest = new ChartParser(TestData2.grammar)
      val result = objectUnderTest.parse(TestData2.incorrectUtterance)
      //then
      result shouldBe empty
    }

  }

}
