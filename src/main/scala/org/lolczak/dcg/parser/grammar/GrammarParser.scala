package org.lolczak.dcg.parser.grammar

import org.lolczak.dcg.model._
import org.lolczak.parsing.lexical._
import org.lolczak.parsing.syntactical.GenericTokenParsers
import org.lolczak.parsing.util.HelperParsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scalaz.{\/-, -\/, \/}

object GrammarParser extends GenericTokenParsers with HelperParsers {

  lazy val languageDef: LanguageDef = LanguageDef(
    commentStart = "/*",
    commentEnd = "*/",
    commentLine = "//",
    identStart = _.isLetter,
    identLetter = x => x.isLetter | x.isDigit | x == '.',
    reservedNames = Set("import"),
    delimiters = Set("[", "]", "=", ",", "?", "->", "|", "<", ">"),
    snippet = Some("{", "}")
  )

  def parseGrammar(content: String): ParseResult[(Lexicon, Nonterminals)] = grammar(new lexical.Scanner(content))

  lazy val importDirective: Parser[List[ImportDirective]] = rep("import" ~> stringLit ^^ {case path => ImportDirective(path)})

  lazy val grammar: Parser[(Lexicon, Nonterminals)] =
    for {
      productions <- rep(production)
      nonterminals = productions filter (_.isRight) map {case \/-(r) => r}
      terminals = productions filter (_.isLeft) map {case -\/(l) => l}
      lexicon = Lexicon.fromProductions(terminals: _*)
    } yield (lexicon, Nonterminals(nonterminals.head.lhs.name, nonterminals))

  lazy val production: Parser[TerminalProduction \/ Production] = terminal ^^ (-\/(_)) | nonterminal ^^ (\/-(_))

  lazy val nonterminal: Parser[Production] =
    lhs ~ repTill(term, productionEnd) ~ opt(guardCode) ^^ { case l ~ r ~ mg => Production(l, r, mg) }

  lazy val guardCode: Parser[String] = codeSnippet

  lazy val terminal: Parser[TerminalProduction] =
    lhs ~ separatedSequence(stringLit, "|", productionEnd) ^^ { case l ~ r => TerminalProduction(l, r)}

  lazy val lhs: Parser[Term] = term <~ "->"

  lazy val productionEnd: Parser[Any] = eoi | guard(lhs) | guard(guardCode)

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

}
