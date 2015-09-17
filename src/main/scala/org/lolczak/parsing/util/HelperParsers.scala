package org.lolczak.parsing.util

import scala.util.parsing.combinator.Parsers

trait HelperParsers { this:Parsers =>

  def repTill[T](p: => Parser[T], end: => Parser[Any]): Parser[List[T]] =
    end ^^^ List.empty | (p ~ repTill(p, end)) ^^ { case x ~ xs => x :: xs }

  def separatedSequence[T](p: => Parser[T], s: => Parser[Any], end: => Parser[Any]): Parser[List[T]] =
    for {
      x  <- p
      xs <- repTill(s ~> p, end)
    } yield x :: xs

}
