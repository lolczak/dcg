package org.lolczak.dcg.loader

import java.io.File

import org.lolczak.dcg.model.{Nonterminals, Grammar}
import org.lolczak.util.Resources
import org.scalatest.{Matchers, WordSpec}

import scalaz.\/-

class GrammarLoaderSpec extends WordSpec with Matchers {

  "Grammar loader" should {

    "follow include directives during loading" in {
      //when
      val result = GrammarLoader.load(new File(Resources.findUrl("include_sample/main.dcg").get.toURI))
      //then
      result should matchPattern {
        case \/-(Grammar(Nonterminals("S", rules), lexicon)) if rules.size == 8 =>
      }
    }

  }

}
