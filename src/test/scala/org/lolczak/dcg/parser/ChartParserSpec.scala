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

  //given

  //when

  //then

}
