package org.lolczak.dcg.parser.language

sealed trait ParseTree[+A,+B]
case class Leaf[+A,+B](value: B) extends ParseTree[A, B]
case class Node[+A,+B](term: A, children: List[ParseTree[A, B]]) extends ParseTree[A, B]
