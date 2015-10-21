package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Production
import org.lolczak.dcg.parser.language.variable.VariableAssignment

import scalaz.{-\/, \/-}

package object expr {

  def evalGuard(production: Production)(unifiedAssignment: VariableAssignment): Option[VariableAssignment] = {
    if (production.containsGuard) {
      val guardCode = production.maybeSnippet.get
      val result = production.exprVal.evalGuard(guardCode, unifiedAssignment)
      result match {
        case -\/(err) => throw new RuntimeException(err.toString)
        case \/-(EvalResult(_, false)) => None
        case \/-(EvalResult(variableAssignment, true)) => Some(variableAssignment)
      }
    } else Some(unifiedAssignment)

  }

}
