package org.lolczak.dcg

object Lexicon {
  private def convert(productions: LexProduction*): Map[String, Set[Term]] = {
    val tuples: Seq[(String, Term)] = productions flatMap (item => item.rhs.map(x => (x, item.lhs)))
    val grouped: Map[String, Seq[(String, Term)]] = tuples groupBy (_._1)
    val untupled: Map[String, Seq[Term]] = grouped mapValues (_.map(_._2))
    untupled mapValues (Set(_: _*))
  }

  def fromProductions(prods: LexProduction*) = new Lexicon(convert(prods: _*))

}

case class Lexicon(private val content: Map[String, Set[Term]]) {

  def this(tuples: (String, Set[Term])*) = this(Map(tuples: _*))

//  def this(prods: LexProduction*) = this(Lexicon.convert(prods: _*))

  def findAllForms(word: String): Set[Term] = content.get(word).getOrElse(Set.empty)

}
