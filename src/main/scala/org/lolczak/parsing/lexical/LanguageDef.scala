package org.lolczak.parsing.lexical

case class LanguageDef(commentStart: String,
                       commentEnd: String,
                       commentLine: String,
                       identStart: Matcher,
                       identLetter: Matcher,
                       reservedNames: Set[String],
                       delimiters: Set[String],
                       snippet: Option[(String, String)] = None)
