package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.Production
import org.lolczak.dcg.parser.language.variable.VariableAssignment

import scalaz.{\/-, -\/}

package object guard {

  def evalGuard(production: Production, guardEval: GuardEval)(unifiedAssignment: VariableAssignment): Option[VariableAssignment] = {
    if (production.containsGuard) {
      val result = guardEval.eval(production.maybeSnippet.get, unifiedAssignment)
      result match {
        case -\/(err) => throw new RuntimeException(err.toString)
        case \/-(EvalResult(_, false)) => None
        case \/-(EvalResult(variableAssignment, true)) => Some(variableAssignment)
      }
    } else Some(unifiedAssignment)

  }

}
