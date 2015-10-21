package org.lolczak.dcg

import org.lolczak.dcg.loader.GrammarLoader
import org.lolczak.dcg.parser.language.ChartParser
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

class GrammarFeaturesSpec extends FeatureSpec with Matchers with GeneratorDrivenPropertyChecks {

  info("Grammar should")

  feature("support for list values as features") {
    val grammarString =
      """
        |NP[PerNum=<per, num>] -> Det[PerNum=<'frst', num>] Noun[PerNum=<'frst', num>] {per = 'frst'}
        |
        |Det[PerNum=<'frst', 'sg'>] -> 'this' | 'that' | 'the'
        |Det[PerNum=<'frst', 'pl'>] -> 'these' | 'those'
        |
        |Noun[PerNum=<'frst', 'sg'>] -> 'plane'
        |Noun[PerNum=<'frst', 'pl'>] -> 'planes'
      """.stripMargin

    val grammar = GrammarLoader.load(grammarString).toOption.get

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
        |NP[PerNum=<per, num>] -> Det[PerNum=<_, num>, f1=_] Noun[PerNum=<_, num>] {per = 'frst'}
        |
        |Det[PerNum=<'frst', 'sg'>, f1='A'] -> 'this' | 'that' | 'the'
        |Det[PerNum=<'frst', 'pl'>, f2='B'] -> 'these' | 'those'
        |
        |Noun[PerNum=<'frst', 'sg'>] -> 'plane'
        |Noun[PerNum=<'frst', 'pl'>] -> 'planes'
      """.stripMargin

    val grammar = GrammarLoader.load(grammarString).toOption.get

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
        |NP[PerNum=pn] -> Det[PerNum=pn] Noun[PerNum=pn]
        |
        |Det[PerNum=[Per='frst', Num='sg']] -> 'this' | 'that' | 'the'
        |Det[PerNum=[Per='frst', Num='pl']] -> 'these' | 'those'
        |
        |Noun[PerNum=[Per='frst', Num='sg']] -> 'plane'
        |Noun[PerNum=[Per='frst', Num='pl']] -> 'planes'
      """.stripMargin

    val grammar = GrammarLoader.load(grammarString).toOption.get

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
        |NP[PerNum=[Per='frst', Num=n]] -> Det[PerNum=[Per='frst', Num=n]] Noun[PerNum=[Per='frst', Num=n]]
        |
        |Det[PerNum=[Per='frst', Num='sg']] -> 'this' | 'that' | 'the'
        |Det[PerNum=[Per='frst', Num='pl']] -> 'these' | 'those'
        |
        |Noun[PerNum=[Per='frst', Num='sg']] -> 'plane'
        |Noun[PerNum=[Per='frst', Num='pl']] -> 'planes'
      """.stripMargin

    val grammar = GrammarLoader.load(grammarString).toOption.get

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

  feature("support for empty productions") {
    scenario("Empty production at the beginning of a rule") {
      //given
      val grammar = GrammarLoader.load(
        """
          |S -> NP VP
          |NP -> empty Noun
          |VP -> Verb
          |
          |empty -> ∅
          |
          |Noun -> 'boy'
          |Verb -> 'runs'
          |
        """.stripMargin).toOption.get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("boy runs")
      //then
      result should have size 1
    }

    scenario("Empty production in the middle of a rule") {
      //given
      val grammar = GrammarLoader.load(
        """
          |S -> NP empty VP
          |NP -> Noun
          |VP -> Verb
          |
          |empty -> (/)
          |
          |Noun -> 'boy'
          |Verb -> 'runs'
          |
        """.stripMargin).toOption.get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("boy runs")
      //then
      result should have size 1
    }

    scenario("Empty production at the end of a rule") {
      //given
      val grammar = GrammarLoader.load(
        """
          |S -> NP VP empty
          |NP -> Noun
          |VP -> Verb
          |
          |empty -> ∅
          |
          |Noun -> 'boy'
          |Verb -> 'runs'
          |
        """.stripMargin).toOption.get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("boy runs")
      //then
      result should have size 1
    }

    scenario("Empty productions everywhere") {
      //given
      val grammar = GrammarLoader.load(
        """
          |S -> empty1 NP empty2 VP empty3
          |NP -> Noun
          |VP -> Verb
          |
          |empty1 -> ∅
          |empty2 -> (/)
          |empty3 -> ∅
          |
          |Noun -> 'boy'
          |Verb -> 'runs'
          |
        """.stripMargin).toOption.get
      //when
      val objectUnderTest = new ChartParser(grammar)
      val result = objectUnderTest.parse("boy runs")
      //then
      result should have size 1
    }

  }

  feature("Support for permutation of rhs terms") {

    scenario("Whole rhs of production is a permutation ") {
      val grammar = GrammarLoader.load(
        """
          |S -> << Noun[num=n, case="nom"] Verb[num=n] Noun[num=_, case="acc"] Noun[num=_, case="dat"] >>
          |
          |Noun[num="sg", case="nom"] -> 'Adam'
          |Noun[num="pl", case="nom"] -> 'Adamowie'
          |Noun[num="sg", case="acc"] -> 'książkę'
          |Noun[num="sg", case="dat"] -> 'właścicielowi'
          |Verb[num="sg"] -> 'daje'
          |
        """.
          stripMargin).toOption.get

      val correctSentences = Gen.oneOf("Adam daje książkę właścicielowi",
        "właścicielowi daje książkę Adam",
        "książkę właścicielowi Adam daje")

      val incorrectSentences = Gen.oneOf("Adamowie daje książkę właścicielowi",
        "właścicielowi daje książkę Adamowie",
        "książkę właścicielowi Adamowie daje")

      forAll(correctSentences) { sentence =>
        //when
        val
        objectUnderTest = new ChartParser(grammar)
        val result = objectUnderTest.parse(sentence)
        //then
        result should have size 1
      }

      forAll(incorrectSentences) { sentence =>
        //when
        val
        objectUnderTest = new ChartParser(grammar)
        val result = objectUnderTest.parse(sentence)
        //then
        result should have size 0
      }
    }

    scenario("A few permutations in production") {
      val grammar = GrammarLoader.load(
        """
          |S -> << A B >> C <<D E>>
          |
          |A -> 'a'
          |B -> 'b'
          |C -> 'c'
          |D -> 'd'
          |E -> 'e'
        """.
          stripMargin).toOption.get

      val correctSentences = Gen.oneOf(
        "a b c d e",
        "b a c d e",
        "b a c e d",
        "a b c e d")

      val incorrectSentences = Gen.oneOf(
        "a a c d e",
        "a b c d c",
        "a b d e")

      forAll(correctSentences) { sentence =>
        //when
        val
        objectUnderTest = new ChartParser(grammar)
        val result = objectUnderTest.parse(sentence)
        //then
        result should have size 1
      }

      forAll(incorrectSentences) { sentence =>
        //when
        val
        objectUnderTest = new ChartParser(grammar)
        val result = objectUnderTest.parse(sentence)
        //then
        result should have size 0
      }
    }

  }


}