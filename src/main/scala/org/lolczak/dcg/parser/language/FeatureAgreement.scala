package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.{FConst, FeatureRhsOperand, FeatureStruct, Term}

object FeatureAgreement {

  def isConsistent(term1: Term, term2: Term): Boolean = {
    term1.name == term2.name && featuresAreConsistent(term1.fStruct, term2.fStruct)
  }

  def featuresAreConsistent(feat1: FeatureStruct, feat2: FeatureStruct): Boolean = {
    val keys = feat1.features.keySet //++ feat2.features.keySet
    val checkConsistency: (FeatureRhsOperand, FeatureRhsOperand) => Boolean = {
      case (FConst(c1), FConst(c2)) => c1 == c2
      case _ => true
    }
    keys.forall { name =>
      val consistent = for {
        v1 <- feat1.features.get(name)
        v2 <- feat2.features.get(name)
      } yield checkConsistency(v1, v2)
      consistent.getOrElse(true)
    }
  }

}
