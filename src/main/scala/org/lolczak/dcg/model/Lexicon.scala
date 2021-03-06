package org.lolczak.dcg.model

import org.lolczak.dcg.parser.grammar.ast.TerminalProduction

object Lexicon {

  private def convert(productions: TerminalProduction*): Map[String, Set[Term]] = {
    val tuples: Seq[(String, Term)] = productions flatMap (item => item.rhs.map(x => (x, Term(item.lhs.name, item.lhs.fStruct))))
    val grouped: Map[String, Seq[(String, Term)]] = tuples groupBy (_._1)
    val untupled: Map[String, Seq[Term]] = grouped mapValues (_.map(_._2))
    untupled mapValues (Set(_: _*))
  }

  def fromProductions(prods: TerminalProduction*): Lexicon = new SimpleLexicon(convert(prods: _*))

}

trait Lexicon {
  self =>

  def findAllForms(word: String): Set[Term]

  def +(that: Lexicon) = new Lexicon {
    override def findAllForms(word: String): Set[Term] = self.findAllForms(word) ++ that.findAllForms(word)
  }

}

case class SimpleLexicon(private val content: Map[String, Set[Term]]) extends Lexicon {

  def this(tuples: (String, Set[Term])*) = this(Map(tuples: _*))

  def findAllForms(word: String): Set[Term] = content.getOrElse(word, Set.empty)

}
