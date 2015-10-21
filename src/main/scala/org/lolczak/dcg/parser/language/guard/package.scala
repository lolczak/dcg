package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Production
import org.lolczak.dcg.parser.language.variable.VariableAssignment

import scalaz.{\/-, -\/}

package object guard {

  def evalGuard(production: Production)(unifiedAssignment: VariableAssignment): Option[VariableAssignment] = {
    if (production.containsGuard) {
      val (snippet, guardEval) = production.maybeSnippet.get
      val result = guardEval.evalGuard(snippet, unifiedAssignment)
      result match {
        case -\/(err) => throw new RuntimeException(err.toString)
        case \/-(EvalResult(_, false)) => None
        case \/-(EvalResult(variableAssignment, true)) => Some(variableAssignment)
      }
    } else Some(unifiedAssignment)

  }

}
