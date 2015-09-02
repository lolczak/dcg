package org.lolczak.dcg.parser

import org.lolczak.dcg.Grammar._
import org.lolczak.dcg._
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _}

import scala.Predef.{augmentString => _, wrapString => _, _}


object TestData {

  val lexicon = new Lexicon(
    "fly" -> Set[Term]("Verb", "Noun"("Num" -> FConst("sg"))),
    "like" -> Set[Term]("Verb", "Prep"),
    "arrow" -> Set[Term]("Noun"("Num" -> FConst("sg"))),
    "these" -> Set[Term]("Det"("Num" -> FConst("pl"))),
    "planes" -> Set[Term]("Noun"("Num" -> FConst("pl"))),
    "an" -> Set[Term]("Det"("Num" -> FConst("sg")))
  )
  
  val grammar = Grammar("S",
    List(
      "S" ~>("NP", "VP"),
      "VP" ~> "Verb",
      "VP" ~>("Verb", "NP"),
      "VP" ~>("VP", "PP"),
      "NP"("Num" -> FVariable("n")) ~> "Noun"("Num" -> FVariable("n")),
      "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n"))),
      "NP"("Num" -> FVariable("n")) ~>("NP"("Num" -> FVariable("n")), "PP"),
      "PP" ~>("Prep", "NP")
    )
  )

}
