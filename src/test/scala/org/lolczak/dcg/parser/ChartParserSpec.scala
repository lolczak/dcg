package org.lolczak.dcg.parser

import org.lolczak.dcg.{Lexicon, Term}
import org.scalatest.{Matchers, WordSpec}

class ChartParserSpec extends WordSpec with Matchers {

  "A scanner" should {

    "build a state containing passive edges representing possible lexemes" in {
      //given
      val lexicon = new Lexicon("fly" -> Set(Term("Noun"), Term("Verb")))
      //when
      val result = ChartParser.scan("fly", 1, lexicon)
      //then
      result shouldBe Set(
        Passive(1, 2, Term("Noun"), Node(Term("Noun"), List(Leaf("fly")))),
        Passive(1, 2, Term("Verb"), Node(Term("Verb"), List(Leaf("fly"))))
      )
    }

  }

  "A predictor" should {

    "create edges for all productions having prefix equal to provided passive edge" in {
      //given
      val edge = Passive(0, 1, Term("Verb"), Leaf("fly"))
      val grammar = TestData.grammar
      //when
      val result = ChartParser.predict(grammar, edge)
      //then
      result should contain only(
        Active(0, 1, Term("VP"), List(Term("NP")), List(Leaf("fly"))),
        Passive(0, 1, Term("VP"), Node(Term("VP"), List(Leaf("fly"))))
        )
    }

  }



  //given

  //when

  //then

}
