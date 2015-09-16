package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FConst, FVariable, FeatureStruct}
import org.scalatest.{Matchers, WordSpec}

class VariableAssignmentSpec extends WordSpec with Matchers {

  "VariableAssignment object" should {
    "find variable by name" in {
      //given
      val substitution = VariableAssignment(
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
      val substitution = VariableAssignment(
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
        val substitution1 = VariableAssignment(
          "x" -> FConst("test1"),
          "y" -> FConst("test2")
        )
        val substitution2 = VariableAssignment(
          "z" -> FConst("test1")
        )
        //when
        val result = substitution1.union(substitution2)
        //then
        result shouldBe Some(
          VariableAssignment(
            "x" -> FConst("test1"),
            "y" -> FConst("test2"),
            "z" -> FConst("test1")
          )
        )
      }

      "substitutions are consistent" in {
        //given
        val substitution1 = VariableAssignment(
          "x" -> FConst("test1"),
          "y" -> FConst("test2")
        )
        val substitution2 = VariableAssignment(
          "x" -> FConst("test1"),
          "z" -> FConst("test1")
        )
        //when
        val result = substitution1.union(substitution2)
        //then
        result shouldBe Some(
          VariableAssignment(
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
        val substitution1 = VariableAssignment(
          "x" -> FConst("test1"),
          "y" -> FConst("test2")
        )
        val substitution2 = VariableAssignment(
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
