package org.lolczak.dcg.loader

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.grammar.GrammarParser
import org.lolczak.dcg.parser.grammar.GrammarParser._
import org.lolczak.dcg.parser.grammar.ast._
import org.lolczak.dcg.parser.language.guard.{GroovyGuardEval, GuardEval}

import scalaz.{\/, -\/, \/-}

object GrammarLoader {

  def load(grammarTxt: String): GrammarFailure \/ Grammar = {
    for {
      grammarAst       <- parse(grammarTxt)
      importDirectives = grammarAst.directives.filter(_.isInstanceOf[ImportDirective]).map(_.asInstanceOf[ImportDirective].file)
      guard            = new GroovyGuardEval(importDirectives) //todo load resources
      lexicon          = Lexicon.fromProductions(grammarAst.terminals: _*)
      productions      = processNonterminals(grammarAst.nonterminals, guard)
      nonterminals     = Nonterminals(productions.head.lhs.name, productions)
    } yield Grammar(nonterminals, lexicon)
  }

  def processNonterminals(nonterminals: List[AstProduction], guard: GuardEval): List[Production] = {
    nonterminals.flatMap { p =>
      val allRhs = permutate(p.rhs)
      val lhs = Term(p.lhs.name, p.lhs.fStruct)
      allRhs.map(rhs => Production(lhs, rhs, p.maybeSnippet.map((_, guard)), p.id))
    }
  }

  def permutate(rhs: List[RhsSymbol]): List[List[Term]] = {
    rhs.foldLeft(List(List.empty[Term])) {
      case (acc, AstTerm(name, struct)) =>
        acc.map(list => list :+ Term(name, struct))
      case (acc, Permutation(terms)) =>
        for {
          prefix <- acc
          tail   <- terms.map(t => Term(t.name, t.fStruct)).permutations.toList
        } yield prefix ++ tail
    }
  }

  def parse(grammarTxt: String): ParsingFailure \/ GrammarAst = {
    GrammarParser.parseGrammar(grammarTxt) match {
      case Success(grammarAst, _) => \/-(grammarAst)
      case fault: NoSuccess       => -\/(ParsingFailure(fault.toString))
    }
  }
}

sealed trait GrammarFailure
case class ParsingFailure(msg: String) extends GrammarFailure
case class CodeLoadFailure(msg: String) extends GrammarFailure
case class ResourceLoadFailure(msg: String) extends GrammarFailure