package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{FeatureItem, FVariable, FeatureZipper}

object FeatureFunctions {

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

}
