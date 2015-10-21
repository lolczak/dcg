package org.lolczak.dcg.parser.language.guard

import org.lolczak.dcg.parser.language.variable.VariableAssignment

import scalaz.\/

trait ExprEval {

  def evalGuard(guardCode: String, unifiedAssignment: VariableAssignment): EvalFailure \/ EvalResult

}

sealed trait EvalFailure
case class CompilationFailure(message: String) extends EvalFailure
case class ExecutionFailure(message: String)   extends EvalFailure
case class BindingFailure(message: String)     extends EvalFailure

case class EvalResult(variableAssignment: VariableAssignment, fulfilled: Boolean)
