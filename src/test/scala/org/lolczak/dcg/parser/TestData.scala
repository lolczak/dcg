package org.lolczak.dcg.parser

import org.lolczak.dcg.model.Grammar._
import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.grammar.GrammarParser.{keyword => _}

import scala.Predef.{augmentString => _, wrapString => _, _}

object TestData {

  val lexicon = new SimpleLexicon(
    "fly" -> Set[Term]("Verb", "Noun"("Num" -> FConst("sg"))),
    "like" -> Set[Term]("Verb", "Prep"),
    "arrow" -> Set[Term]("Noun"("Num" -> FConst("sg"))),
    "these" -> Set[Term]("Det"("Num" -> FConst("pl"))),
    "this" -> Set[Term]("Det"("Num" -> FConst("sg"))),
    "planes" -> Set[Term]("Noun"("Num" -> FConst("pl"))),
    "an" -> Set[Term]("Det"("Num" -> FConst("sg")))
  )
  
  val nonterminals = Nonterminals("S",
    List(
      "S" ~>("NP", "VP"),
      "VP" ~> "Verb",
      "VP" ~>("Verb", "NP"),
      "VP" ~>("VP", "PP"),
      "ERule" ~> ("Empty", "Sth"),
      "Empty" ~> (),
      "NP"("Num" -> FVariable("n")) ~> "Noun"("Num" -> FVariable("n")),
      "NP"("Num" -> FVariable("n")) ~>("Det"("Num" -> FVariable("n")), "Noun"("Num" -> FVariable("n"))),
      "NP"("Num" -> FVariable("n")) ~>("NP"("Num" -> FVariable("n")), "PP"),
      "PP" ~>("Prep", "NP")
    )
  )

  val grammar = Grammar(nonterminals, lexicon, List.empty)

  val utterance = "these planes fly like an arrow"

  val incorrectUtterance = "this planes fly like an arrow"

}
