package org.lolczak.dcg.parser.language.guard

import groovy.lang.{Binding, GroovyShell}
import org.lolczak.dcg.parser.language.variable.VariableAssignment

import scalaz.{\/-, \/}

class GroovyGuardEval extends GuardEval {
  override def eval(guardCode: String, unifiedAssignment: VariableAssignment): EvalFailure \/ EvalResult = {

    val sharedData = new Binding()
    unifiedAssignment.forEach { case (varName, value) => sharedData.setVariable(varName, value.toString) }
    val shell = new GroovyShell(sharedData)
    val result = shell.evaluate(guardCode)
    \/-(EvalResult(unifiedAssignment, result.toString.toBoolean))
  }
}
