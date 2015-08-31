package org.lolczak.dcg.parser

import org.lolczak.dcg.Term

sealed trait Edge
case class Passive(start:Int, end: Int, found: Term, tree: ParseTree) extends Edge
case class Active(start:Int, end: Int, found: Term, remaining: List[Term], parsedPrefix: List[ParseTree]) extends Edge
