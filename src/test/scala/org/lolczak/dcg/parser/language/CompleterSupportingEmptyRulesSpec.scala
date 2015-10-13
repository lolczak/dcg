package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Grammar._
import org.lolczak.dcg.model.Term
import org.lolczak.dcg.parser.TestData
import org.scalatest.{Matchers, WordSpec}

import scalaz.{\/-, -\/}

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

    //todo "complete empty rules"

  }

}
