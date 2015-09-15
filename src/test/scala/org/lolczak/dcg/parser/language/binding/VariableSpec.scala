package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.Grammar._
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _}
import org.lolczak.dcg.parser.language.Node
import org.lolczak.dcg.{FConst, FVariable}
import org.scalatest.{Matchers, WordSpec}

import scala.Predef.{augmentString => _, wrapString => _, _}

class VariableSpec extends WordSpec with Matchers {

  "Unification algorithm" should {

    "substitute all variables" in {
      //given
      val production = "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n")))
      val parsedTerms = List(Node("Det"("Num" -> FConst("pl")), List.empty), Node("Noun"("Num" -> FConst("pl")), List.empty))
      //when
      val result = Variable.unify(production, parsedTerms)
      //then
      result shouldBe Some(
        "NP"("Num" -> FConst("pl"))
      )
    }

    "return None" when {
      "there is missing variable assignment" in {
        //given
        val production = "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FConst("pl")), "Noun"("Num" -> FConst("pl")))
        val parsedTerms = List(Node("Det"("Num" -> FConst("pl")), List.empty), Node("Noun"("Num" -> FConst("pl")), List.empty))
        //when
        val result = Variable.unify(production, parsedTerms)
        //then
        result shouldBe None
      }
    }

  }
}
