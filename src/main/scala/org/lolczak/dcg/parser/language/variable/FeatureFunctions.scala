package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.language.expr.ExprEval

object FeatureFunctions {

  //todo test it

  /**
   * Finds variables path inside the FeatureItem.
   *
   * @param root
   * @return
   */
  def findVariables(root: FeatureItem): List[FeatureZipper] =
    FeatureZipper.filter {
      case _: FVariable => true
      case _ => false
    }(root)

  def containsVariables(root: FeatureItem): Boolean = findVariables(root).nonEmpty

  /**
   * Substitutes all variables in feature item. Variable values are taken from the substitution object.
   *
   * @param root
   * @param substitution
   * @return
   */
  def substitute(root: FeatureItem, substitution: VariableAssignment): FeatureItem = {
    findVariables(root).map(_.breadcrumbs).foldLeft(root) {
      case (item, path) => FeatureZipper.alter { case v@FVariable(name) => substitution.find(name).getOrElse(item) } (path) (item)
    }
  }

  def findExpressions(root: FeatureItem): List[FeatureZipper] =
    FeatureZipper.filter {
      case _: FExpr => true
      case _ => false
    }(root)

  def containsExpressions(root: FeatureItem): Boolean = findExpressions(root).nonEmpty

  def evalAllExpressions(root: FeatureItem, substitution: VariableAssignment, exprEval: ExprEval): FeatureItem = {
    findExpressions(root).map(_.breadcrumbs).foldLeft(root) {
      case (item, path) => FeatureZipper.alter {
        case e@FExpr(code) => exprEval.evalExpr(code, substitution).fold(
          l = failure => throw new RuntimeException(s"Cannot evaluate $code. Error occurred: $failure"),
          r = value   => FConst(value))
      } (path) (item)
    }
  }

}
