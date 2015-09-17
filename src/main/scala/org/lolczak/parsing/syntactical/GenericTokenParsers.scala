package org.lolczak.parsing.syntactical

import org.lolczak.parsing.lexical.{GenericLexer, GenericTokens, LanguageDef}

import scala.util.parsing.combinator.syntactical.StdTokenParsers

abstract class GenericTokenParsers extends StdTokenParsers {
  val languageDef: LanguageDef
  override type Tokens = GenericTokens
  override val lexical = new GenericLexer(languageDef)

  import lexical.{Delimiter, SnippetCode}

  def delimiter: Parser[String] =
    elem("delimiter", _.isInstanceOf[Delimiter]) ^^ (_.chars)

  def codeSnippet: Parser[String] =
    elem("code snippet", _.isInstanceOf[SnippetCode]) ^^ (_.chars)

  override implicit def keyword(chars: String): Parser[String] =
    super.keyword(chars) | (accept(Delimiter(chars)) ^^^ chars)
}
