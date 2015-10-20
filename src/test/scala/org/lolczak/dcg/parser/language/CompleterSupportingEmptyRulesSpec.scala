package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar._
import org.lolczak.dcg.model.{Grammar, Nonterminals, Term}
import org.lolczak.dcg.parser.testData
import org.scalatest.{Matchers, WordSpec}

import scalaz.{-\/, \/-}

class CompleterSupportingEmptyRulesSpec extends WordSpec with Matchers {

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
      val objectUnderTest = CompleterSupportingEmptyRules
      val result = objectUnderTest.complete(chart)(edge)
      //then
      result should contain only(
        -\/(Active(0, 2, Term("VP"), List(Term("PP")), List(Node(Term("AA"), List.empty), Node(Term("Verb"), List.empty)), testProduction2)),
        \/-(PassiveCandidate(0, 2, testProduction1, List(Node(Term("AA"), List.empty), Node(Term("Verb"), List.empty))))
        )
    }

    "match empty production inside active edge" in {
      //given
      val grammar = Grammar(
        Nonterminals("S",
          List(
            "S" ~>("NP", "VP"),
            "VP" ~>("Verb", "Empty", "NP"),
            "Empty" ~>()
          )
        ), testData.lexicon)
      val edge = Active(0, 1, Term("VP"), List("Empty", "NP"), List(Node("Verb", List.empty)), "VP" ~>("Verb", "Empty", "NP"))
      //when
      val objectUnderTest = CompleterSupportingEmptyRules
      val result = objectUnderTest.completeEmpty(grammar)(edge)
      //then
      result should contain only
        -\/(Active(0, 1, Term("VP"), List("NP"), List(Node(Term("Empty"), List(Leaf("∅"))), Node(Term("Verb"), List.empty)), "VP" ~>("Verb", "Empty", "NP")))
    }

  }

}
