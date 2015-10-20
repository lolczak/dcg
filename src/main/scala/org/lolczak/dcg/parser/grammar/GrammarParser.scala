package org.lolczak.dcg.parser.grammar

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.grammar.ast.Production
import org.lolczak.dcg.parser.grammar.ast.Term
import org.lolczak.dcg.parser.grammar.ast.TerminalProduction
import org.lolczak.dcg.parser.grammar.ast._
import org.lolczak.parsing.lexical._
import org.lolczak.parsing.syntactical.GenericTokenParsers
import org.lolczak.parsing.util.HelperParsers

import scalaz.{-\/, \/, \/-}

object GrammarParser extends GenericTokenParsers with HelperParsers {

  lazy val languageDef: LanguageDef = LanguageDef(
    commentStart = "/*",
    commentEnd = "*/",
    commentLine = "//",
    identStart = _.isLetter,
    identLetter = x => x.isLetter | x.isDigit | x == '.',
    reservedNames = Set("import", "id"),
    delimiters = Set("[", "]", "=", ",", "?", "->", "|", "<", ">", "_", "(/)", "∅", "#"),
    snippet = Some("{", "}")
  )

  def parseGrammar(content: String): ParseResult[GrammarAst] = grammar(new lexical.Scanner(content))

  lazy val importDirectives: Parser[List[ImportDirective]] = rep("import" ~> stringLit ^^ {case path => ImportDirective(path)})

  lazy val grammar: Parser[GrammarAst] =
    for {
      directives   <- importDirectives
      productions  <- rep(production)
      nonterminals = productions filter (_.isRight) map { case \/-(r) => r }
      terminals    = productions filter (_.isLeft)  map { case -\/(l) => l }
//      lexicon      = Lexicon.fromProductions(terminals: _*)
    } yield GrammarAst(directives, nonterminals, terminals)

  lazy val production: Parser[TerminalProduction \/ Production] = terminal ^^ (-\/(_)) | nonterminal ^^ (\/-(_))

  lazy val nonterminal: Parser[Production] =
    ( opt(prodId) ~ lhs ~ repTill(term, productionEnd) ~ opt(guardCode) ^^ { case id ~ l ~ r ~ mg => Production(l, r, mg, id) }
    | (opt(prodId) ~ lhs <~ emptyRhs ^^ { case id ~ l => Production(l, List.empty, None, id) }))

  lazy val prodId: Parser[String] = "#" ~ "id" ~ "=" ~> stringLit

  lazy val emptyRhs: Parser[Any] = "(/)" | "∅"

  lazy val guardCode: Parser[String] = codeSnippet

  lazy val terminal: Parser[TerminalProduction] =
    lhs ~ separatedSequence(stringLit, "|", productionEnd) ^^ { case l ~ r => TerminalProduction(l, r)}

  lazy val lhs: Parser[Term] = term <~ "->"

  lazy val productionEnd: Parser[Any] = eoi | guard(lhs) | guard(guardCode) | guard(prodId)

  lazy val term: Parser[Term] =
    for {
      symbolName <- ident
      maybeFStruct <- opt(featureStruct)
    } yield Term(symbolName, maybeFStruct.getOrElse(FeatureStruct.empty))

  lazy val featureStruct: Parser[FeatureStruct] =
    "[" ~> separatedSequence(feature, featureSeparator, featureEnd) ^^ { f => FeatureStruct(Map(f: _*)) }

  lazy val feature: Parser[(String, FeatureItem)] = (ident <~ "=") ~ (fvariable | fvalue | flist | fplaceholder | featureStruct) ^^ { case name ~ fval => (name, fval) }

  lazy val fvariable: Parser[FVariable] = ident ^^ { varName => FVariable(varName) }

  lazy val fvalue: Parser[FConst] = stringLit ^^ { varName => FConst(varName) }

  lazy val flist: Parser[FList] = "<" ~> separatedSequence(fvariable | fvalue | fplaceholder, ",",  ">" ) ^^ { case list => FList(list)}

  lazy val fplaceholder: Parser[FPlaceholder.type] = "_" ^^^ FPlaceholder

  lazy val featureSeparator: Parser[Unit] = "," ^^^()

  lazy val featureEnd: Parser[Unit] = "]" ^^^()

  lazy val eoi: Parser[Any] = Parser { in => if (in.atEnd) Success((), in) else Failure("not end", in) }

}
