package org.lolczak.dcg.parser

import org.lolczak.dcg.loader.GrammarLoader
import org.lolczak.dcg.model.Term
import org.lolczak.dcg.parser.grammar.GrammarParser._
import org.lolczak.dcg.parser.language.{Leaf, Node, ChartParser}
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class ParserSpec  extends WordSpec with Matchers {

  val grammarString = Source.fromURL(Thread.currentThread().getContextClassLoader.getResource("gram_guards.dcg"), "UTF-8").mkString
  val grammar = GrammarLoader.load(grammarString).toOption.get

  val objectUnderTest = new ChartParser(grammar)

  "A parser " should {

    "parse grammar and utterance" in {
      val result = objectUnderTest.parse(testData.utterance)
      result should have size 1
      result.head should matchPattern {
        case Node(Term("S", _), List(
        Node(Term("NP", _), List(
          Node(Term("Det", _), List(Leaf("these")), _),
          Node(Term("Noun", _), List(Leaf("planes")), _)), _
        ),
        Node(Term("VP", _), List(
          Node(Term("VP", _), List(
            Node(Term("Verb", _), List(Leaf("fly")), _)
        ), _),
          Node(Term("PP", _), List(
            Node(Term("Prep", _), List(Leaf("like")), _),
            Node(Term("NP", _), List(
              Node(Term("Det", _), List(Leaf("an")), _),
              Node(Term("Noun", _), List(Leaf("arrow")), _)
        ), _)
        ), _)), _)), _
        ) =>
      }
    }

  }

}
