package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import Grammar._
import org.lolczak.dcg.parser.TestData
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _}
import org.lolczak.dcg.parser.language.ChartParser.Chart
import org.lolczak.dcg.parser.language.guard.GroovyGuardEval
import org.scalatest.{Matchers, WordSpec}

import scala.Predef.{augmentString => _, wrapString => _, _}

class ChartParserSpec extends WordSpec with Matchers {

  "A scanner" should {

    "build a state containing passive edges representing possible lexemes" in {
      //given
      val lexicon = new Lexicon("fly" -> Set(Term("Noun"), Term("Verb")))
      //when
      val result = ChartParser.scan("fly", 1, lexicon)
      //then
      result shouldBe State(Set(
        Passive(1, 2, Term("Noun"), Node(Term("Noun"), List(Leaf("fly")))),
        Passive(1, 2, Term("Verb"), Node(Term("Verb"), List(Leaf("fly"))))
      ))
    }

  }

  "A predictor" should {

    "create edges for all productions having prefix equal to provided passive edge" in {
      //given
      val edge = Passive(0, 1, Term("Verb"), Node(Term("Verb"), List(Leaf("fly"))))
      val grammar = TestData.grammar
      //when
      val result = ChartParser.predict(grammar, edge, new GroovyGuardEval)
      //then
      result should contain only(
        Active(0, 1, Term("VP"), List(Term("NP")), List(Node(Term("Verb"), List(Leaf("fly")))), "VP" ~>("Verb", "NP")),
        Passive(0, 1, Term("VP"), Node(Term("VP"), List(Node(Term("Verb"), List(Leaf("fly"))))))
        )
    }

  }

  "A completer" should {
    "combine active nodes with passive ones" in {
      //given
      val edge = Passive(1, 2, Term("Verb"), Node(Term("Verb"), List.empty))
      val testProduction1 = "VP" ~>("AA", "Verb")
      val testProduction2 = "VP" ~>("AA", "Verb", "PP")
      val chart: Chart = IndexedSeq(State(Set(
        Active(0, 1, Term("VP"), List(Term("Verb")), List(Node(Term("AA"), List.empty)), testProduction1),
        Active(0, 1, Term("VP"), List(Term("Verb"), Term("PP")), List(Node(Term("AA"), List.empty)), testProduction2)
      )), State(Set()))
      //when
      val result = ChartParser.combine(chart, edge, new GroovyGuardEval)
      //then
      result should contain only(
        Active(0, 2, Term("VP"), List(Term("PP")), List(Node(Term("AA"), List.empty), Node(Term("Verb"), List.empty)), testProduction2),
        Passive(0, 2, Term("VP"), Node(Term("VP"), List(Node(Term("AA"), List.empty), Node(Term("Verb"), List.empty))))
        )
    }

  }

  "A parser" should {

    "match rhs terms with matching features" in {
      //given
      val grammar = Grammar("S",
        List(
          "NP"("Num" -> FConst("pl")) ~>("Det"("Num" -> FConst("pl")), "Noun"("Num" -> FConst("pl"))),
          "NP"("Num" -> FConst("sg")) ~>("Det"("Num" -> FConst("sg")), "Noun"("Num" -> FConst("sg"))),
          "PP" ~>("Prep", "NP")
        )
      )
      //when
      val result = ChartParser.parseDcg(grammar, TestData.lexicon, "these planes", Some("NP"))
      //then
      val ExpectedFeatures = FeatureStruct(Map("Num" -> FConst("pl")))
      result should have size 1
      result.head should matchPattern {
        case Node(Term("NP", ExpectedFeatures), _) =>
      }
    }

    "bind variables when they are consistent" in {
      //given
      val grammar = Grammar("NP",
        List(
          "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n"))),
          "PP" ~>("Prep", "NP")
        )
      )
      //when
      val result = ChartParser.parseDcg(grammar, TestData.lexicon, "these planes", Some("NP"))
      //then
      val ExpectedFeatures = FeatureStruct(Map("Num" -> FConst("pl")))
      result should have size 1
      result.head should matchPattern {
        case Node(Term("NP", ExpectedFeatures), _) =>
      }
    }

    "find parse tree for correct utterance" in {
      //when
      val result = ChartParser.parseDcg(TestData.grammar, TestData.lexicon, TestData.utterance)
      //then
      result should have size 1
      result.head should matchPattern {
        case Node(Term("S", _), List(Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf(these))), Node(Term("Noun", _), List(Leaf("planes"))))), Node(Term("VP", _), List(Node(Term("VP", _), List(Node(Term("Verb", _), List(Leaf(fly))))), Node(Term("PP", _), List(Node(Term("Prep", _), List(Leaf("like"))), Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf("an"))), Node(Term("Noun", _), List(Leaf("arrow"))))))))))) =>
      }
    }

    "return empty list for incorrrect utterance" in {
      //when
      val result = ChartParser.parseDcg(TestData.grammar, TestData.lexicon, TestData.incorrectUtterance)
      //then
      result shouldBe empty
    }

  }

}
