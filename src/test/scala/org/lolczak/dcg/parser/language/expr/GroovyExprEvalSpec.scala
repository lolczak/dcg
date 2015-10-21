package org.lolczak.dcg.parser.language.expr

import org.lolczak.dcg.model.FConst
import org.lolczak.dcg.parser.language.variable.VariableAssignment
import org.lolczak.util.Resources
import org.scalatest.{Matchers, WordSpec}

import scalaz.{-\/, \/-}

class GroovyExprEvalSpec extends WordSpec with Matchers {

  "Groovy eval" should {

    "test if constraint are fulfilled" in {
      //given
      val TestAssignment = VariableAssignment("x" -> FConst("2"), "y" -> FConst("3"))
      val guardCode = "x != y"
      //when
      val objectUnderTest = new GroovyExprEval
      val result = objectUnderTest.evalGuard(guardCode, TestAssignment)
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
      val objectUnderTest = new GroovyExprEval
      val result = objectUnderTest.evalGuard(guardCode, TestAssignment)
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
      val objectUnderTest = new GroovyExprEval
      val result = objectUnderTest.evalGuard(guardCode, TestAssignment)
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
      val objectUnderTest = new GroovyExprEval
      val result = objectUnderTest.evalGuard(guardCode, TestAssignment)
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
      val objectUnderTest = new GroovyExprEval
      val result = objectUnderTest.evalGuard(guardCode, TestAssignment)
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
      val objectUnderTest = new GroovyExprEval(Resources.load("functions.groovy").get)
      val result = objectUnderTest.evalGuard(guardCode, TestAssignment)
      //then
      result should matchPattern {
        case \/-(EvalResult(ExpectedAssignment, true)) =>
      }
    }

    "evaluate expression" in {
      //given
      val exprCode = "r = asInt(x) + asInt(y); r.toString()"
      //when
      val objectUnderTest = new GroovyExprEval(Resources.load("functions.groovy").get)
      val result = objectUnderTest.evalExpr(exprCode, VariableAssignment("x" -> FConst("2"), "y" -> FConst("3")))
      //then
      result shouldBe \/-("5")
    }

//    "return cast failure if returned value has wrong type" in {
//      //given
//      val exprCode = "r = asInt(x) + asInt(y)"
//      //when
//      val objectUnderTest = new GroovyExprEval(Resources.load("functions.groovy").get)
//      val result = objectUnderTest.evalExpr[VariableAssignment](exprCode, VariableAssignment("x" -> FConst("2"), "y" -> FConst("3")))
//      //then
//      result should matchPattern { case -\/(CastFailure(_)) => }
//    }

  }

}
