package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._
import Grammar._
import org.lolczak.dcg.parser.TestData
import org.lolczak.dcg.parser.grammar.GrammarParser
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _}
import org.lolczak.dcg.parser.language.guard.GroovyGuardEval
import org.scalatest.{Matchers, WordSpec}

import scala.Predef.{augmentString => _, wrapString => _, _}

class ChartParserSpec extends WordSpec with Matchers {

  "A scanner" should {

    "build a state containing passive edges representing possible lexemes" in {
      //given
      val lexicon = new Lexicon("fly" -> Set(Term("Noun"), Term("Verb")))
      //when
      val objectUnderTest = new ChartParser(Grammar(TestData.nonterminals,lexicon))
      val result = objectUnderTest.scan("fly", 1, lexicon)
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
      //when
      val objectUnderTest = new ChartParser(Grammar(TestData.nonterminals, TestData.lexicon))
      val result = objectUnderTest.predict(TestData.nonterminals, edge)
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
      val chart = IndexedSeq(State(Set(
        Active(0, 1, Term("VP"), List(Term("Verb")), List(Node(Term("AA"), List.empty)), testProduction1),
        Active(0, 1, Term("VP"), List(Term("Verb"), Term("PP")), List(Node(Term("AA"), List.empty)), testProduction2)
      )), State(Set()))
      //when
      val objectUnderTest = new ChartParser(TestData.grammar)
      val result = objectUnderTest.combine(chart, edge)
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
      val nonterminals = Nonterminals("S",
        List(
          "NP"("Num" -> FConst("pl")) ~>("Det"("Num" -> FConst("pl")), "Noun"("Num" -> FConst("pl"))),
          "NP"("Num" -> FConst("sg")) ~>("Det"("Num" -> FConst("sg")), "Noun"("Num" -> FConst("sg"))),
          "PP" ~>("Prep", "NP")
        )
      )
      //when
      val objectUnderTest = new ChartParser(Grammar(nonterminals, TestData.lexicon),new GroovyGuardEval, Some("NP"))
      val result = objectUnderTest.parse("these planes")
      //then
      val ExpectedFeatures = FeatureStruct(Map("Num" -> FConst("pl")))
      result should have size 1
      result.head should matchPattern {
        case Node(Term("NP", ExpectedFeatures), _) =>
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
      val objectUnderTest = new ChartParser(Grammar(nonterminals, TestData.lexicon),new GroovyGuardEval, Some("NP"))
      val result = objectUnderTest.parse("these planes")
      //then
      val ExpectedFeatures = FeatureStruct(Map("Num" -> FConst("pl")))
      result should have size 1
      result.head should matchPattern {
        case Node(Term("NP", ExpectedFeatures), _) =>
      }
    }

    "support list values" in {
      import scala.Predef.augmentString
      //given
      val grammarString =
        """
          |NP[PerNum=<?per, ?num>] -> Det[PerNum=<'frst', ?num>] Noun[PerNum=<'frst', ?num>] {per = 'frst'}
          |
          |Det[PerNum=<'frst', 'sg'>] -> 'this' | 'that' | 'the'
          |Det[PerNum=<'frst', 'pl'>] -> 'these' | 'those'
          |
          |Noun[PerNum=<'frst', 'sg'>] -> 'plane'
          |Noun[PerNum=<'frst', 'pl'>] -> 'planes'
        """.stripMargin

      val grammar = GrammarParser.parseGrammar(grammarString).get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      val result2 = objectUnderTest.parse("this planes")
      //then
      result should have size 1
      result2 shouldBe empty
    }

    "support placeholders" in {
      import scala.Predef.augmentString
      //given
      val grammarString =
        """
          |NP[PerNum=<?per, ?num>] -> Det[PerNum=<_, ?num>, f1=_] Noun[PerNum=<_, ?num>] {per = 'frst'}
          |
          |Det[PerNum=<'frst', 'sg'>, f1='A'] -> 'this' | 'that' | 'the'
          |Det[PerNum=<'frst', 'pl'>, f2='B'] -> 'these' | 'those'
          |
          |Noun[PerNum=<'frst', 'sg'>] -> 'plane'
          |Noun[PerNum=<'frst', 'pl'>] -> 'planes'
        """.stripMargin

      val grammar = GrammarParser.parseGrammar(grammarString).get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      val result2 = objectUnderTest.parse("this planes")
      //then
      result should have size 1
      result2 shouldBe empty
    }

    "support nested feature structs" in {
      import scala.Predef.augmentString
      //given
      val grammarString =
        """
          |NP[PerNum=?pn] -> Det[PerNum=?pn] Noun[PerNum=?pn]
          |
          |Det[PerNum=[Per='frst', Num='sg']] -> 'this' | 'that' | 'the'
          |Det[PerNum=[Per='frst', Num='pl']] -> 'these' | 'those'
          |
          |Noun[PerNum=[Per='frst', Num='sg']] -> 'plane'
          |Noun[PerNum=[Per='frst', Num='pl']] -> 'planes'
        """.stripMargin

      val grammar = GrammarParser.parseGrammar(grammarString).get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      val result2 = objectUnderTest.parse("this planes")
      //then
      result should have size 1
      result2 shouldBe empty
    }

    "support binding inside nested feature structs" in {
      import scala.Predef.augmentString
      //given
      val grammarString =
        """
          |NP[PerNum=[Per='frst', Num=?n]] -> Det[PerNum=[Per='frst', Num=?n]] Noun[PerNum=[Per='frst', Num=?n]]
          |
          |Det[PerNum=[Per='frst', Num='sg']] -> 'this' | 'that' | 'the'
          |Det[PerNum=[Per='frst', Num='pl']] -> 'these' | 'those'
          |
          |Noun[PerNum=[Per='frst', Num='sg']] -> 'plane'
          |Noun[PerNum=[Per='frst', Num='pl']] -> 'planes'
        """.stripMargin

      val grammar = GrammarParser.parseGrammar(grammarString).get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      val result2 = objectUnderTest.parse("this planes")
      //then
      result should have size 1
      result2 shouldBe empty
    }

    "find parse tree for correct utterance" in {
      //when
      val objectUnderTest = new ChartParser(TestData.grammar)
      val result = objectUnderTest.parse(TestData.utterance)
      //then
      result should have size 1
      result.head should matchPattern {
        case Node(Term("S", _), List(Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf(these))), Node(Term("Noun", _), List(Leaf("planes"))))), Node(Term("VP", _), List(Node(Term("VP", _), List(Node(Term("Verb", _), List(Leaf(fly))))), Node(Term("PP", _), List(Node(Term("Prep", _), List(Leaf("like"))), Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf("an"))), Node(Term("Noun", _), List(Leaf("arrow"))))))))))) =>
      }
    }

    "return empty list for incorrrect utterance" in {
      //when
      val objectUnderTest = new ChartParser(TestData.grammar)
      val result = objectUnderTest.parse(TestData.incorrectUtterance)
      //then
      result shouldBe empty
    }

  }

}
