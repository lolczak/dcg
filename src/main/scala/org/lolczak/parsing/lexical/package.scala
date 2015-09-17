package org.lolczak.parsing

package object lexical {
  type Matcher = Char => Boolean

  val letterMatcher: Matcher = _.isLetter
}
