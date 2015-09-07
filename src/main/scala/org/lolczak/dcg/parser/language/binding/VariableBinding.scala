package org.lolczak.dcg.parser.language.binding

import org.lolczak.dcg.FeatureValue

case class VariableBinding(varName: String, value: FeatureValue)

case class VariableAssignment(varName: String, value: FeatureValue)
