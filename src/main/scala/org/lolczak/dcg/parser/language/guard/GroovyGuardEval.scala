package org.lolczak.dcg.parser.language.guard

import java.net.URL

import groovy.lang.{GroovyClassLoader, Binding, GroovyShell, Script}
import org.lolczak.dcg.model.FConst
import org.lolczak.dcg.parser.language.variable.VariableAssignment
import org.lolczak.util.Resources

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Try
import scalaz.\/

class GroovyGuardEval(imports: List[String] = List.empty) extends GuardEval {

  //todo move to loader
  private val importedSnippets: List[URL] = imports.map(x => Resources.findUrl(x).getOrElse(throw new RuntimeException(s"Cannot load $x")))

  override def eval(guardCode: String, unifiedAssignment: VariableAssignment): EvalFailure \/ EvalResult = {
    val sharedData = new Binding()
    unifiedAssignment.forEach { case (varName, value) => sharedData.setVariable(varName, value.toString) }
    for {
      script <- compile(guardCode)
      result <- run(script, sharedData)
      variables = extractVariables(sharedData, unifiedAssignment)
      success = Try(result.toString.toBoolean).getOrElse(true)
    } yield EvalResult(variables, success)
  }

  private def compile(guardCode: String): EvalFailure \/ Script = {
    val code = importedSnippets.map(Source.fromURL(_).mkString) :+ guardCode mkString "\n"
    val shell = new GroovyShell()
    \/.fromTryCatchNonFatal(shell.parse(code)) leftMap { case th => CompilationFailure(th.getMessage) }
  }

  private def run(script: Script, sharedData: Binding): EvalFailure \/ Any = {
    script.setBinding(sharedData)
    \/.fromTryCatchNonFatal(script.run()) leftMap { case th => ExecutionFailure(th.getMessage) }
  }

  private def extractVariables(sharedData: Binding, unifiedAssignment: VariableAssignment): VariableAssignment = {
    val allNames: Set[String] = sharedData.getVariables.keySet().toSet[Any].map(_.toString)
    val names: Set[String] = allNames -- unifiedAssignment.variables
    names.foldLeft(unifiedAssignment) {
      case (acc, varName) => acc.put(varName, FConst(sharedData.getVariable(varName).toString))
    }
  }

}
