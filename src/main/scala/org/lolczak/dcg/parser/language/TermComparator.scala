package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.{FConst, FeatureStruct, FeatureZipper, Term}

object TermComparator {

  /**
   * Check if terms matches initially.
   *
   * @param term1
   * @param term2
   * @return
   */
  def isConsistent(term1: Term, term2: Term): Boolean =
    term1.name == term2.name && featuresAreConsistent(term1.fStruct, term2.fStruct)

  /**
   * Check if term constants are the same.
   *
   * @param feat1
   * @param feat2
   * @return
   */
  def featuresAreConsistent(feat1: FeatureStruct, feat2: FeatureStruct): Boolean = {
    FeatureZipper.filter(_.isInstanceOf[FConst])(feat1) forall {
      case FeatureZipper(FConst(left), path) =>
        FeatureZipper.goto(path)(feat2).map {
          case FConst(right) => right == left
          case _             => true
        } getOrElse true
    }
  }

}

class TermOps(ruleTerm: Term) {

  def matches(parsedTerm: Term): Boolean = TermComparator.isConsistent(ruleTerm, parsedTerm)

}

object ToTermOps {

  implicit def toTermOps(term: Term) = new TermOps(term)

}