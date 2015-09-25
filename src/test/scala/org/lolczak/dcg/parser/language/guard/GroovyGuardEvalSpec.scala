package org.lolczak.dcg.parser.language.guard

import org.lolczak.dcg.model.FConst
import org.lolczak.dcg.parser.language.variable.VariableAssignment
import org.scalatest.{Matchers, WordSpec}

import scalaz.{-\/, \/-}

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

    "assign variable" in {
      //given
      val TestAssignment = VariableAssignment("x" -> FConst("2"), "y" -> FConst("3"))
      val ExpectedAssignment = TestAssignment.add("z", FConst("5")).get
      val guardCode = "z = x.toInteger() + y.toInteger()"
      //when
      val objectUnderTest = new GroovyGuardEval
      val result = objectUnderTest.eval(guardCode, TestAssignment)
      //then
      result should matchPattern {
        case \/-(EvalResult(ExpectedAssignment, true)) =>
      }
    }

    "both assign variable and test constraints" in {
      //given
      val TestAssignment = VariableAssignment("x" -> FConst("2"), "y" -> FConst("3"))
      val ExpectedAssignment = TestAssignment.add("z", FConst("23")).get
      val guardCode = "z = x + y; x != y && z == x + y"
      //when
      val objectUnderTest = new GroovyGuardEval
      val result = objectUnderTest.eval(guardCode, TestAssignment)
      //then
      result should matchPattern {
        case \/-(EvalResult(ExpectedAssignment, true)) =>
      }
    }

    "return compilation failure when snippet doesn't compile" in {
      //given
      val TestAssignment = VariableAssignment("x" -> FConst("2"), "y" -> FConst("3"))
      val guardCode = "234 423$#@$@# $#@ $23"
      //when
      val objectUnderTest = new GroovyGuardEval
      val result = objectUnderTest.eval(guardCode, TestAssignment)
      //then
      result should matchPattern {
        case -\/(CompilationFailure(_)) =>
      }
    }

    "return execution failure when runtime error occurs" in {
      //given
      val TestAssignment = VariableAssignment("x" -> FConst("2"), "y" -> FConst("3"))
      val guardCode = "z = qwerty"
      //when
      val objectUnderTest = new GroovyGuardEval
      val result = objectUnderTest.eval(guardCode, TestAssignment)
      //then
      result should matchPattern {
        case -\/(ExecutionFailure(_)) =>
      }
    }

    "include in script imported files" in {
      //given
      val TestAssignment = VariableAssignment("x" -> FConst("2"), "y" -> FConst("3"))
      val ExpectedAssignment = TestAssignment.add("z", FConst("5")).get
      val guardCode = "z = asInt(x) + asInt(y)"
      //when
      val objectUnderTest = new GroovyGuardEval(List("functions.groovy"))
      val result = objectUnderTest.eval(guardCode, TestAssignment)
      //then
      result should matchPattern {
        case \/-(EvalResult(ExpectedAssignment, true)) =>
      }
    }

  }

}
