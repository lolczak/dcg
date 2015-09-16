package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FConst, FVariable, FeatureStruct}
import org.scalatest.{Matchers, WordSpec}

class SubstitutionSpec extends WordSpec with Matchers {

  "A substitution factory method" should {
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
        val result = Substitution.fromFeatures(ruleFeatures, parsedFeatures)
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
        val result = Substitution.fromFeatures(ruleFeatures, parsedFeatures)
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
        val result = Substitution.fromFeatures(ruleFeatures, parsedFeatures)
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
        val result = Substitution.fromFeatures(ruleFeatures, parsedFeatures)
        //then
        result shouldBe Some(
          Substitution(
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
        val result = Substitution.fromFeatures(ruleFeatures, parsedFeatures)
        //then
        result shouldBe Some(
          Substitution(
            "x" -> FConst("test1")
          )
        )
      }
    }
  }

  "A substitution object" should {
    "find variable by name" in {
      //given
      val substitution = Substitution(
        "x" -> FConst("test1"),
        "y" -> FConst("test2")
      )
      //when
      val result = substitution.find("x")
      //then
      result shouldBe Some(FConst("test1"))
    }

    "return None if there is no substitution for variable name" in {
      //given
      val substitution = Substitution(
        "x" -> FConst("test1"),
        "y" -> FConst("test2")
      )
      //when
      val result = substitution.find("z")
      //then
      result shouldBe None
    }

    "combine two substitution" when {
      "there is no conflicts" in {
        //given
        val substitution1 = Substitution(
          "x" -> FConst("test1"),
          "y" -> FConst("test2")
        )
        val substitution2 = Substitution(
          "z" -> FConst("test1")
        )
        //when
        val result = substitution1.union(substitution2)
        //then
        result shouldBe Some(
          Substitution(
            "x" -> FConst("test1"),
            "y" -> FConst("test2"),
            "z" -> FConst("test1")
          )
        )
      }

      "substitutions are consistent" in {
        //given
        val substitution1 = Substitution(
          "x" -> FConst("test1"),
          "y" -> FConst("test2")
        )
        val substitution2 = Substitution(
          "x" -> FConst("test1"),
          "z" -> FConst("test1")
        )
        //when
        val result = substitution1.union(substitution2)
        //then
        result shouldBe Some(
          Substitution(
            "x" -> FConst("test1"),
            "y" -> FConst("test2"),
            "z" -> FConst("test1")
          )
        )
      }
    }

    "not combine two substitution" when {
      "there is conflict" in {
        //given
        val substitution1 = Substitution(
          "x" -> FConst("test1"),
          "y" -> FConst("test2")
        )
        val substitution2 = Substitution(
          "x" -> FConst("test1"),
          "y" -> FConst("test3")
        )
        //when
        val result = substitution1.union(substitution2)
        //then
        result shouldBe None
      }
    }

  }

}
