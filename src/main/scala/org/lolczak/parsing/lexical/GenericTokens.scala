package org.lolczak.parsing.lexical

import scala.util.parsing.combinator.token.StdTokens

trait GenericTokens extends StdTokens {

  case class SnippetCode(chars: String) extends Token {
    override def toString = "{"+chars+"}"
  }

  case class Delimiter(chars: String) extends Token {
    override def toString = chars
  }

}
