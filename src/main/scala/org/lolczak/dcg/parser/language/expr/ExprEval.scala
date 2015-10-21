package org.lolczak.dcg.parser.language.expr

import org.lolczak.dcg.parser.language.variable.VariableAssignment

import scalaz.\/

trait ExprEval {

  def evalGuard(guardCode: String, unifiedAssignment: VariableAssignment): EvalFailure \/ EvalResult

  def evalExpr(exprCode: String, assignment: VariableAssignment): EvalFailure \/ String

}

sealed trait EvalFailure
case class CompilationFailure(message: String) extends EvalFailure
case class ExecutionFailure(message: String)   extends EvalFailure
case class BindingFailure(message: String)     extends EvalFailure
case class CastFailure(message: String)        extends EvalFailure

case class EvalResult(variableAssignment: VariableAssignment, fulfilled: Boolean)