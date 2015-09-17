package org.lolczak.parsing.syntactical

import org.lolczak.parsing.lexical.{LanguageDef, GenericLexer, GenericTokens}

import scala.util.parsing.combinator.syntactical.StdTokenParsers

abstract class GenericTokenParsers extends StdTokenParsers {
  val languageDef: LanguageDef
  override type Tokens = GenericTokens
  override val lexical = new GenericLexer(languageDef)

  import lexical.{SnippetCode, Delimiter}

  def delimiter: Parser[String] =
    elem("delimiter", _.isInstanceOf[Delimiter]) ^^ (_.chars)

  def codeSnippet: Parser[String] =
    elem("code snippet", _.isInstanceOf[SnippetCode]) ^^ (_.chars)

}
