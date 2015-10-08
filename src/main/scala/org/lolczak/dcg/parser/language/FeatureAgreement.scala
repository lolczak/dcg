package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._

object FeatureAgreement {

  def isConsistent(term1: Term, term2: Term): Boolean = {
    term1.name == term2.name && featuresAreConsistent(term1.fStruct, term2.fStruct)
  }

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
