package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar._
import org.lolczak.dcg.model.{Grammar, Term}
import org.lolczak.dcg.parser.TestData
import org.scalatest.{Matchers, WordSpec}

import scalaz.{-\/, \/-}

class PredictorSupportingEmptyRulesSpec extends WordSpec with Matchers {

  "A predictor" should {

    "create passive edge candidate for all productions having prefix equal to provided passive edge" in {
      //given
      val edge = Passive(0, 1, Term("Verb"), Node(Term("Verb"), List(Leaf("fly"))))
      val grammar = Grammar(TestData.nonterminals, TestData.lexicon)
      //when
      val objectUnderTest = PredictorSupportingEmptyRules
      val result = objectUnderTest.predict(grammar)(edge)
      //then
      result should contain only(
        \/-(PassiveCandidate(edge.start, edge.end, "VP" ~> "Verb", List(Node(Term("Verb"), List(Leaf("fly")))))),
        -\/(Active(0, 1, Term("VP"), List(Term("NP")), List(Node(Term("Verb"), List(Leaf("fly")))), "VP" ~>("Verb", "NP")))
        )
    }

    "create passive edge candidate for productions having as a prefix empty rules and first non-empty term is equal to provided passive edge" in {
      //given
      val edge = Passive(0, 1, Term("Sth"), Node(Term("Sth"), List(Leaf("something"))))
      val grammar = Grammar(TestData.nonterminals, TestData.lexicon)
      //when
      val objectUnderTest = PredictorSupportingEmptyRules
      val result = objectUnderTest.predict(grammar)(edge)
      //then
      result should contain only
        \/-(PassiveCandidate(edge.start, edge.end,  "ERule" ~> ("Empty", "Sth"), List(Node("Empty", List(Leaf("∅"))), Node(Term("Sth"), List(Leaf("something"))))))

    }

  }

}
