package org.lolczak.dcg.parser

import org.lolczak.dcg.model.Term
import org.lolczak.dcg.parser.grammar.GrammarParser._
import org.lolczak.dcg.parser.language.{Leaf, Node, ChartParser}
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class ParserSpec  extends WordSpec with Matchers {

  "A parser " should {

    "parse grammar and utterance" in {
      val grammarString = Source.fromURL(Thread.currentThread().getContextClassLoader.getResource("gram_guards.dcg"), "UTF-8").mkString
      val (lexicon, grammar) = parseGrammar(grammarString).get
      val result = ChartParser.parseDcg(grammar, lexicon, TestData.utterance)
      result should have size 1
      result.head should matchPattern {
        case Node(Term("S", _), List(Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf(these))), Node(Term("Noun", _), List(Leaf("planes"))))), Node(Term("VP", _), List(Node(Term("VP", _), List(Node(Term("Verb", _), List(Leaf(fly))))), Node(Term("PP", _), List(Node(Term("Prep", _), List(Leaf("like"))), Node(Term("NP", _), List(Node(Term("Det", _), List(Leaf("an"))), Node(Term("Noun", _), List(Leaf("arrow"))))))))))) =>
      }
    }

  }

}
