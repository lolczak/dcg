package org.lolczak.parsing.lexical

import org.lolczak.parsing.util.HelperParsers

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharArrayReader._

class GenericLexer(languageDef: LanguageDef) extends Lexical with GenericTokens with HelperParsers {

  import languageDef._

  override def token: Parser[Token] =
    (   identifier
      | numeric
      | string
      | EofCh                                             ^^^ EOF
      | '\'' ~> failure("unclosed string literal")
      | '\"' ~> failure("unclosed string literal")
      | delim
      | codeSnippet
      | failure("illegal character")
      )

  def numeric: Parser[NumericLit] =
    digit ~ rep(digit) ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }

  def string: Parser[StringLit] = (
      '\'' ~ rep( chrExcept('\'', '\n', EofCh) ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "") }
    | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    )

  def whitespace: Parser[Any] = rep(
      whitespaceChar
    | charSeq(commentStart) ~ comment
    | charSeq(commentLine)  ~ rep( chrExcept(EofCh, '\n') )
    | charSeq(commentStart) ~ failure("unclosed comment")
  )

  def charSeq(str: String) = acceptSeq(str.toList)

  protected def comment: Parser[Any] =
      repTill(chrExcept (EofCh) , acceptSeq(commentEnd.toList)) ^^ { case _ => ' ' }

  def identifier: Parser[Token] =
    elem("identStart", identStart) ~ rep(elem("identStart", identLetter)) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }

  protected def processIdent(name: String) =
    if (languageDef.reservedNames contains name) Keyword(name) else Identifier(name)

  protected def delim: Parser[Token] = {
    val parsers = delimiters.map(charSeq(_) ^^ {case chars => Delimiter(chars.mkString)})
    parsers.foldRight(failure("no matching delimiter"): Parser[Token])((x,y) => x | y)
  }

  protected def codeSnippet: Parser[Token] =
    snippet.map { case (start,end) =>
      charSeq(start) ~ repTill(chrExcept (EofCh) , charSeq(end)) ^^ { case s ~ body => SnippetCode(body.mkString) }
    } getOrElse(failure("cs"))

}
