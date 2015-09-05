package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg._
import org.lolczak.dcg.parser.language.{Node, ParseTree, Passive}

import scalaz.{-\/, \/, \/-}

case class VarAssignments(assignments: Map[String, FValue]) {

  //  def +(varName: String, value: FValue): VarAssignments = ???

  def combine(that: VarAssignments): VarAssignments.Error \/ VarAssignments = {
    val varNames: Set[String] = this.assignments.keySet ++ that.assignments.keySet
    val tuples: Set[(String, Option[FValue], Option[FValue])] = for {
      varName <- varNames
      left  = this.assignments.get(varName)
      right = that.assignments.get(varName)
    } yield (varName, left, right)
    val inconsistents = tuples.find {
      case (varName, left, right) =>
        val diff = for (l<-left; r<-right) yield (l!=r)
        diff.getOrElse(false)
    } map (_._1)
    if (inconsistents.nonEmpty) -\/(s"Inconsistency for $inconsistents")
    else {
      val pairs: Set[(String, FValue)] = tuples.map(x=>(x._1, x._2.orElse(x._2).get))
      val map: Map[String, FValue] = pairs.foldLeft(Map.empty[String, FValue]) {
        case (acc, pair) => acc.+(pair)
      }
      \/-(VarAssignments(map))
    }
  }

  /**
   * Finds value for variable.
   *
   * @param varName
   * @return
   */
  def find(varName: String): Option[FConst] = {
    val result = assignments.get(varName)
    result flatMap {
      case value: FConst => Some(value)
      case _ => None //todo handle list
    }
  }

}

object VarAssignments {

  type Error = String

  val empty = VarAssignments(Map.empty)

  def createPassive(start: Int, end: Int, production: Production, parsedTerms: List[ParseTree[Term, String]]): Option[Passive] = {
    val rhs = production.rhs
    val parsedRhs = parsedTerms.map { case Node(term, _) => term }
    if (rhs.length != parsedRhs.length) None
    else {
      val zipped = rhs zip parsedRhs
      val varAss = zipped map { case (rule, parsed) => fromFeatures(rule.fStruct, parsed.fStruct) }
      val finalAss: Error \/ VarAssignments = varAss.foldLeft[Error \/ VarAssignments](\/-(empty)) {
        case (prevResult, item) => prevResult.flatMap(prev => prev.combine(item))
      }
      //todo loging add to validator
      val edge = for {
        assignment <- finalAss
        bindedFeatures <- bind(production.lhs.fStruct, assignment)
        term = Term(production.lhs.name, bindedFeatures)
        tree = Node(term, parsedTerms)
      } yield Passive(start, end, term, tree)


      edge fold(l = (msg => None), r = Some(_))
    }
  }

  /**
   * Creates variable assignments.
   *
   * @param ruleFeatures
   * @param parsedFeatures
   * @return
   */
  def fromFeatures(ruleFeatures: FeatureStruct, parsedFeatures: FeatureStruct): VarAssignments = {
    val varNames = ruleFeatures.features.filter(_._2.isVariable) map { case (featName, FVariable(varName)) => (varName, featName) }
    val assignments: Map[String, FValue] = for {
      (varName, featName) <- varNames
      if parsedFeatures.features.contains(featName) && !parsedFeatures.features(featName).isVariable//todo encapsulate it
    } yield (varName, parsedFeatures.features(featName))

    VarAssignments(assignments)
  }

  def bind(termFeatures: FeatureStruct, assignments: VarAssignments): Error \/ FeatureStruct = {

    val map = termFeatures.features.foldLeft[Error \/ Map[String, FValue]](\/-(Map.empty)) {
      case (result@ -\/(_), _) => result
      case (\/-(feats), (feature, FVariable(varName))) =>
        val value = assignments.find(varName)
        value.fold[Error \/ Map[String, FValue]](-\/(s"Cannot find value for variable $varName"))(value => \/-(feats.+((feature, value))))
      case (\/-(feats), tuple) => \/-(feats.+(tuple))
    }
    map.map(FeatureStruct(_))
  }


}
