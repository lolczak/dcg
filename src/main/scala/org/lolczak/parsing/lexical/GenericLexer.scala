package org.lolczak.parsing.lexical

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharArrayReader._

class GenericLexer(languageDef: LanguageDef) extends Lexical with GenericTokens {

  override def token: Parser[Token] = ???

  // see `whitespace in `Scanners`
  def whitespace: Parser[Any] = rep[Any](
    whitespaceChar
      | '/' ~ '*' ~ comment
      | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
      | '/' ~ '*' ~ failure("unclosed comment")
  )

  protected def comment: Parser[Any] = (
      rep (chrExcept (EofCh, '*')) ~ '*' ~ '/'     ^^ { case _ => ' ' }
    | rep (chrExcept (EofCh, '*')) ~ '*' ~ comment ^^ { case _ => ' ' }
    )

  def identifier: Parser[Token] =
    elem("identStart", languageDef.identStart) ~ rep(elem("identStart", languageDef.identLetter)) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }

  protected def processIdent(name: String) =
    if (languageDef.reservedNames contains name) Keyword(name) else Identifier(name)

}
