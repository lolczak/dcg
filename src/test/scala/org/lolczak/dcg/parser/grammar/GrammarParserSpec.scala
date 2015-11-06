package org.lolczak.dcg.parser.grammar

import org.lolczak.dcg.parser.grammar.ast.ops._
import org.lolczak.dcg.model.{Term => _, Production => _, _}
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _, _}
import org.lolczak.dcg.parser.grammar.ast._
import org.scalatest.{Matchers, WordSpec}

import scala.Predef.{augmentString => _, wrapString => _, _}
import scala.io.Source

class GrammarParserSpec extends WordSpec with Matchers {

  "Directive parser" should {

    "parse list of import directives" in {
      import scala.Predef.augmentString
      //given
      val directiveTxt =
        """
          |import "functions.groovy"
          |import "/home/abs/fun.groovy"
        """.stripMargin
      //when
      val result = importDirectives(new GrammarParser.lexical.Scanner(directiveTxt))
      //then
      result should matchPattern {
        case Success(List(ImportDirective("functions.groovy"), ImportDirective("/home/abs/fun.groovy")), _) =>
      }
    }

  }


  "Term parser" should {
    "parse simple term containing only symbol" in {
      //given
      val termString = "Verb"
      //when
      val result = term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern { case Success(AstTerm("Verb", FeatureStruct.empty), _) => }
    }

    "parse term containing feature" in {
      //given
      val termString = "Verb[Per='frst']"
      val ExpectedFStruct = FeatureStruct(Map("Per" -> FConst("frst")))
      //when
      val result = term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern { case Success(AstTerm("Verb", ExpectedFStruct), _) => }
    }

    "parse term containing features" in {
      //given
      val termString = "Verb[Per='frst', Num=n, Cas=<'Acc', v>]"
      val ExpectedFStruct = FeatureStruct(Map("Per" -> FConst("frst"), "Num" -> FVariable("n"), "Cas" -> FList(List(FConst("Acc"), FVariable("v")))))
      //when
      val result = term(new GrammarParser.lexical.Scanner(termString))
      //then
      result should matchPattern { case Success(AstTerm("Verb", ExpectedFStruct), _) => }
    }

  }

  "Nonterminal production parser" should {
    "parse simple nonterminal production" in {
      //given
      val nonterminalString = "S -> NP VP"
      val ExpectedProduction = "S" ~>("NP", "VP")
      //when
      val result = nonterminal(new GrammarParser.lexical.Scanner(nonterminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }

    "parse nonterminal production containing features" in {
      //given
      val nonterminalString = "NP[Num=n, Gen='male'] -> Det[Num=n] Noun[Num=n] "
      val ExpectedProduction = "NP"("Num" -> FVariable("n"), "Gen" -> FConst("male")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n")))
      //when
      val result = nonterminal(new GrammarParser.lexical.Scanner(nonterminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }

    "parse nonterminal productions containing code snippets" in {
      //given
      val nonterminalString = "S -> NP VP { x => y }"
      val ExpectedProduction = "S" ~>("NP", "VP") copy(maybeSnippet = Some(" x => y "))
      //when
      val result = nonterminal(new GrammarParser.lexical.Scanner(nonterminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }

    "parse permutation of terms" in {
      //given
      val nonterminalString = "S -> <<NP VP>>"
      val ExpectedProduction = AstProduction("S", List(Permutation(List("NP", "VP"))))
      //when
      val result = nonterminal(new GrammarParser.lexical.Scanner(nonterminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }

    "parse expressions in lhs terms" in {
      //given
      val nonterminalString = "NP[Num={n}] -> Det[Num=n] Noun[Num=n] "
      val ExpectedProduction = "NP"("Num" -> FExpr("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n")))
      //when
      val result = nonterminal(new GrammarParser.lexical.Scanner(nonterminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }

  }

  "Terminal production parser" should {
    "parse simple terminal production" in {
      //given
      val terminalString = "Noun -> 'test'"
      val ExpectedProduction = TerminalProduction("Noun", List("test"))
      //when
      val result = terminal(new GrammarParser.lexical.Scanner(terminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }

    "parse feature based terminal production" in {
      //given
      val terminalString = "Noun[Num='sg', Gen='male'] -> 'test' | 'test2' "
      val ExpectedProduction = TerminalProduction("Noun"("Num" -> FConst("sg"), "Gen" -> FConst("male")), List("test", "test2"))
      //when
      val result = terminal(new GrammarParser.lexical.Scanner(terminalString))
      //then
      result should matchPattern { case Success(ExpectedProduction, _) => }
    }

  }

  "Grammar parser" should {

    "parse whole grammar" in {
      //given
      val grammarString = Source.fromURL(Thread.currentThread().getContextClassLoader.getResource("feature_based_gram.dcg"), "UTF-8").mkString
      val Terminals = List(
        TerminalProduction("Verb", List("fly")),
        TerminalProduction("Verb", List("like")),
        TerminalProduction("Noun"("Num" -> FConst("pl")), List("planes")),
        TerminalProduction("Noun"("Num" -> FConst("sg")), List("arrow", "fly")),
        TerminalProduction("Det"("Num" -> FConst("sg")), List("an")),
        TerminalProduction("Det"("Num" -> FConst("pl")), List("these")),
        TerminalProduction("Prep", List("like"))
      )
      val ExpectedGrammar =
        List(
          "S" ~>("NP", "VP"),
          "VP" ~> "Verb",
          "VP" ~>("Verb", "NP"),
          "VP" ~>("VP", "PP"),
          "NP"("Num" -> FVariable("n")) ~> "Noun"("Num" -> FVariable("n")),
          "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n"))),
          "NP"("Num" -> FVariable("n")) ~>("NP"("Num" -> FVariable("n")), "PP"),
          "PP" ~>("Prep", "NP")
        )
      //when
      val result = parseGrammar(grammarString)
      //then
      result should matchPattern { case Success(GrammarAst(List(), ExpectedGrammar, Terminals), _) => }
    }

    "parse whole grammar with guards" in {
      //given
      val grammarString = Source.fromURL(Thread.currentThread().getContextClassLoader.getResource("gram_guards.dcg"), "UTF-8").mkString
      val Terminals = List(
        TerminalProduction("Verb", List("fly")),
        TerminalProduction("Verb", List("like")),
        TerminalProduction("Noun"("Num" -> FConst("pl")), List("planes")),
        TerminalProduction("Noun"("Num" -> FConst("sg")), List("arrow", "fly")),
        TerminalProduction("Det"("Num" -> FConst("sg")), List("an")),
        TerminalProduction("Det"("Num" -> FConst("pl")), List("these")),
        TerminalProduction("Prep", List("like"))
      )
      val ExpectedGrammar =
        List(
          "S" ~>("NP", "VP"),
          "VP" ~> "Verb" copy(id=Some("VP1")),
          "VP" ~>("Verb", "NP") copy(id=Some("VP2")),
          "VP" ~>("VP", "PP") copy(id=Some("VP3")),
          "NP"("Num" -> FVariable("n")) ~> "Noun"("Num" -> FVariable("n")) copy(id=Some("NP1")),
          "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n1")), "Noun"("Num" -> FVariable("n2"))) copy(maybeSnippet = Some("n=n1; n1==n2"), id=Some("NP2")),
          "NP"("Num" -> FVariable("n")) ~>("NP"("Num" -> FVariable("n")), "PP") copy(id=Some("NP3")),
          "PP" ~>("Prep", "NP") copy(id=Some("PP1"))
        )
      //when
      val result = parseGrammar(grammarString)
      //then
      result should matchPattern { case Success(GrammarAst(List(ImportDirective("functions.groovy")), ExpectedGrammar, Terminals), _) => }
    }


  }

}
