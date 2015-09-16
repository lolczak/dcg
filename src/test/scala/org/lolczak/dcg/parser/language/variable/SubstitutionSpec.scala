package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FeatureStruct, FConst, FVariable, Grammar}
import Grammar._
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _}
import org.lolczak.dcg.parser.language.Node
import org.scalatest.{Matchers, WordSpec}

import scala.Predef.{augmentString => _, wrapString => _, _}

class SubstitutionSpec extends WordSpec with Matchers {

  "Substitution algorithm" should {

    "substitute all variables" in {
      //given
      val production = "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n")))
      val parsedTerms = List(Node("Det"("Num" -> FConst("pl")), List.empty), Node("Noun"("Num" -> FConst("pl")), List.empty))
      //when
      val result = Substitution.substitute(production, parsedTerms)
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
        val result = Substitution.substitute(production, parsedTerms)
        //then
        result shouldBe None
      }
    }

    "not create substitution" when {
      "parsed feature is a variable" in {
        //given
        val ruleFeatures = FeatureStruct(Map(
          "feat1" -> FVariable("test1"),
          "feat2" -> FVariable("test2")
        ))
        val parsedFeatures = FeatureStruct(Map(
          "feat1" -> FConst("test1"),
          "feat2" -> FVariable("n")
        ))
        //when
        val result = Substitution.findAssignment(ruleFeatures, parsedFeatures)
        //then
        result shouldBe None
      }
      "there is conflict in parsed features" in {
        //given
        val ruleFeatures = FeatureStruct(Map(
          "feat1" -> FVariable("test"),
          "feat2" -> FVariable("test")
        ))
        val parsedFeatures = FeatureStruct(Map(
          "feat1" -> FConst("test1"),
          "feat2" -> FConst("test2")
        ))
        //when
        val result = Substitution.findAssignment(ruleFeatures, parsedFeatures)
        //then
        result shouldBe None
      }

      "there is no feature in parsed struct" in {
        //given
        val ruleFeatures = FeatureStruct(Map(
          "feat1" -> FVariable("x"),
          "feat2" -> FVariable("y")
        ))
        val parsedFeatures = FeatureStruct(Map(
          "feat1" -> FConst("test1")
        ))
        //when
        val result = Substitution.findAssignment(ruleFeatures, parsedFeatures)
        //then
        result shouldBe None
      }

    }

    "create most general substitution" when {
      "all parsed features are values" in {
        //given
        val ruleFeatures = FeatureStruct(Map(
          "feat1" -> FVariable("x"),
          "feat2" -> FConst("test"),
          "feat3" -> FVariable("y"),
          "feat4" -> FConst("test")
        ))
        val parsedFeatures = FeatureStruct(Map(
          "feat1" -> FConst("test1"),
          "feat2" -> FConst("test"),
          "feat3" -> FConst("test2"),
          "feat4" -> FConst("test")
        ))
        //when
        val result = Substitution.findAssignment(ruleFeatures, parsedFeatures)
        //then
        result shouldBe Some(
          VariableAssignment(
            "x" -> FConst("test1"),
            "y" -> FConst("test2")
          )
        )
      }

      "there is no conflict in parse features" in {
        //given
        val ruleFeatures = FeatureStruct(Map(
          "feat1" -> FVariable("x"),
          "feat2" -> FConst("test"),
          "feat3" -> FVariable("x"),
          "feat4" -> FConst("test")
        ))
        val parsedFeatures = FeatureStruct(Map(
          "feat1" -> FConst("test1"),
          "feat2" -> FConst("test"),
          "feat3" -> FConst("test1"),
          "feat4" -> FConst("test")
        ))
        //when
        val result = Substitution.findAssignment(ruleFeatures, parsedFeatures)
        //then
        result shouldBe Some(
          VariableAssignment(
            "x" -> FConst("test1")
          )
        )
      }
    }


  }
}
