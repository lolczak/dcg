package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model._

object FeatureAgreement {

  def isConsistent(term1: Term, term2: Term): Boolean = {
    term1.name == term2.name && featuresAreConsistent(term1.fStruct, term2.fStruct)
  }

  def featuresAreConsistent(feat1: FeatureStruct, feat2: FeatureStruct): Boolean = { //todo use breadcrumps
    val keys = feat1.features.keySet //++ feat2.features.keySet
    val checkConsistency: (FeatureItem, FeatureItem) => Boolean = { //todo check it
      case (FConst(c1), FConst(c2)) => c1 == c2
      case (FList(c1), FList(c2)) => c1 zip c2 forall {
        case (FConst(f1), FConst(f2)) => f1 == f2
        case _ => true
      }
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
