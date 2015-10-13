package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.{Grammar, Term, SimpleLexicon}
import org.lolczak.dcg.parser.TestData
import org.scalatest.{Matchers, WordSpec}

class SimpleScannerSpec extends WordSpec with Matchers {

  "A scanner" should {

    "build a state containing passive edges representing possible lexemes" in {
      //given
      val lexicon = new SimpleLexicon("fly" -> Set(Term("Noun"), Term("Verb")))
      //when
      val objectUnderTest = SimpleScanner
      val result = objectUnderTest.scan(Grammar(TestData.nonterminals,lexicon))(List("fly"))
      //then
      result shouldBe IndexedSeq(State(Set(
        Passive(0, 1, Term("Noun"), Node(Term("Noun"), List(Leaf("fly")))),
        Passive(0, 1, Term("Verb"), Node(Term("Verb"), List(Leaf("fly"))))
      )))
    }

  }
}
