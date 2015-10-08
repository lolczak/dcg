package org.lolczak.dcg

import org.lolczak.dcg.parser.grammar.GrammarParser
import org.lolczak.dcg.parser.language.ChartParser
import org.scalatest.{Matchers, FeatureSpec}

class GrammarFeaturesSpec extends FeatureSpec with Matchers {

  info("Grammar should")

  feature("support for list values as features") {
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

    scenario("Correct utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      //then
      result should have size 1
    }

    scenario("Incorrect utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("this planes")
      //then
      result shouldBe empty
    }

  }

  feature("support for placeholders") {

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

    scenario("Correct utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      //then
      result should have size 1
    }

    scenario("Incorrect utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("this planes")
      //then
      result shouldBe empty
    }
  }

  feature("support nested feature structs") {

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

    scenario("Correct utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      //then
      result should have size 1
    }

    scenario("Incorrect utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("this planes")
      //then
      result shouldBe empty
    }

  }

  feature("support binding inside nested feature structs") {

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

    scenario("Correct utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("these planes")
      val result2 = objectUnderTest.parse("this planes")
      //then
      result should have size 1
      result2 shouldBe empty
    }

    scenario("Incorrect utterance") {
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("this planes")
      //then
      result shouldBe empty
    }

  }


}