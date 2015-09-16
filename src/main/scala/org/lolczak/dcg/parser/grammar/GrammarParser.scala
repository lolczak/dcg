package org.lolczak.dcg.parser.grammar

import org.lolczak.dcg._
import org.lolczak.dcg.model._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scalaz.{\/-, -\/, \/}

object GrammarParser extends StandardTokenParsers {
  lexical.delimiters ++= List("{", "}", "[", "]", "=", ",", "?", "->", "|")

  def parseGrammar(content: String): ParseResult[(Lexicon, Grammar)] = grammar(new lexical.Scanner(content))

  lazy val grammar: Parser[(Lexicon, Grammar)] =
    for {
      productions <- rep(production)
      nonterminals = productions filter (_.isRight) map {case \/-(r) => r}
      terminals = productions filter (_.isLeft) map {case -\/(l) => l}
      lexicon = Lexicon.fromProductions(terminals: _*)
    } yield (lexicon, Grammar(nonterminals.head.lhs.name, nonterminals))

  lazy val production: Parser[LexProduction \/ Production] = terminal ^^ (-\/(_)) | nonterminal ^^ (\/-(_))

  lazy val nonterminal: Parser[Production] =
    lhs ~ repTill(term, productionEnd) ^^ { case l ~ r => Production(l, r) }

  lazy val terminal: Parser[LexProduction] =
    lhs ~ separatedSequence(stringLit, "|", productionEnd) ^^ { case l ~ r => LexProduction(l, r)}

  lazy val lhs: Parser[Term] = term <~ "->"

  lazy val productionEnd: Parser[Any] = eoi | guard(lhs)

  lazy val term: Parser[Term] =
    for {
      symbolName <- ident
      maybeFStruct <- opt(featureStruct)
    } yield Term(symbolName, maybeFStruct.getOrElse(FeatureStruct.empty))

  lazy val featureStruct: Parser[FeatureStruct] =
    "[" ~> separatedSequence(feature, featureSeparator, featureEnd) ^^ { f => FeatureStruct(Map(f: _*)) }

  lazy val feature: Parser[(String, FeatureRhsOperand)] = (ident <~ "=") ~ (fvariable | fvalue) ^^ { case name ~ fval => (name, fval) }

  lazy val fvariable: Parser[FVariable] = "?" ~> ident ^^ { varName => FVariable(varName) }

  lazy val fvalue: Parser[FConst] = ident ^^ { varName => FConst(varName) }

  lazy val featureSeparator: Parser[Unit] = "," ^^^()

  lazy val featureEnd: Parser[Unit] = "]" ^^^()

  lazy val eoi: Parser[Any] = Parser { in => if (in.atEnd) Success((), in) else Failure("not end", in) }

  def repTill[T](p: => Parser[T], end: => Parser[Any]): Parser[List[T]] =
    end ^^^ List.empty | (p ~ repTill(p, end)) ^^ { case x ~ xs => x :: xs }

  def separatedSequence[T](p: => Parser[T], s: => Parser[Any], end: => Parser[Any]): Parser[List[T]] =
    for {
      x <- p
      xs <- repTill(s ~> p, end)
    } yield x :: xs

}
