package org.lolczak.dcg.parser

import org.lolczak.dcg.Term

sealed trait Edge {
  val isActive: Boolean
  val isPassive: Boolean
}

case object NullEdge extends Edge {
  override val isActive: Boolean = false
  override val isPassive: Boolean = false
}

case class Passive(start: Int, end: Int, found: Term, tree: ParseTree[Term, String]) extends Edge {
  override val isActive: Boolean = false
  override val isPassive: Boolean = true
}

case class Active(start: Int, end: Int, lhs: Term, remaining: List[Term], parsedPrefix: List[ParseTree[Term, String]]) extends Edge {
  override val isActive: Boolean = true
  override val isPassive: Boolean = false
}
