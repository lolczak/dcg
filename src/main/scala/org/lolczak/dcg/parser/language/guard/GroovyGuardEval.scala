package org.lolczak.dcg.parser.language.guard

import groovy.lang.{Binding, GroovyShell}
import org.lolczak.dcg.model.FConst
import org.lolczak.dcg.parser.language.variable.VariableAssignment

import scala.collection.JavaConversions._
import scala.util.Try
import scalaz.{\/, \/-}

class GroovyGuardEval extends GuardEval {

  override def eval(guardCode: String, unifiedAssignment: VariableAssignment): EvalFailure \/ EvalResult = {
    val sharedData = new Binding()
    unifiedAssignment.forEach { case (varName, value) => sharedData.setVariable(varName, value.toString) }
    val shell = new GroovyShell()
    val script = shell.parse(guardCode)
    script.setBinding(sharedData)
    val result = script.run()
    val variables = extractVariables(sharedData, unifiedAssignment)
    val success = Try(result.toString.toBoolean).getOrElse(true)
    \/-(EvalResult(variables, success))
  }

  private def extractVariables(sharedData: Binding, unifiedAssignment: VariableAssignment): VariableAssignment = {
    val allNames: Set[String] = sharedData.getVariables.keySet().toSet[Any].map(_.toString)
    val names: Set[String] = allNames -- unifiedAssignment.variables
    names.foldLeft(unifiedAssignment) {
      case (acc, varName) => acc.put(varName, FConst(sharedData.getVariable(varName).toString))
    }
  }

}
