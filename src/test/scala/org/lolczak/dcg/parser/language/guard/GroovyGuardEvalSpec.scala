package org.lolczak.dcg.parser.language.guard

import org.lolczak.dcg.model.FConst
import org.lolczak.dcg.parser.language.variable.VariableAssignment
import org.scalatest.{Matchers, WordSpec}

import scalaz.\/-

class GroovyGuardEvalSpec extends WordSpec with Matchers {

  "Groovy eval" should {

    "test if constraint are fulfilled" in {
      //given
      val TestAssignment = VariableAssignment("x" -> FConst("2"), "y" -> FConst("3"))
      val guardCode = "x != y"
      //when
      val objectUnderTest = new GroovyGuardEval
      val result = objectUnderTest.eval(guardCode, TestAssignment)
      //then
      result should matchPattern {
        case \/-(EvalResult(TestAssignment, true)) =>
      }
    }

  }

}
