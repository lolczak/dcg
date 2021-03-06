package org.lolczak.dcg.parser.language

import org.lolczak.dcg.model.{Term, Production}

sealed trait Edge {
//  val start: Int
//  val end: Int
  val isActive: Boolean
  val isPassive: Boolean
}

case class Passive(start: Int, end: Int, found: Term, tree: ParseTree[Term, String]) extends Edge {
  override val isActive: Boolean = false
  override val isPassive: Boolean = true
}

case class Active(start: Int, end: Int, lhs: Term, remaining: List[Term], parsedPrefix: List[ParseTree[Term, String]], production: Production) extends Edge {
  override val isActive: Boolean = true
  override val isPassive: Boolean = false
}

case class PassiveCandidate(start: Int, end: Int, production: Production, parsedTerms: List[ParseTree[Term, String]])