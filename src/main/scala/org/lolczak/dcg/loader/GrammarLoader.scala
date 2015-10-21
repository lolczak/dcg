package org.lolczak.dcg.loader

import java.io.File

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.grammar.GrammarParser
import org.lolczak.dcg.parser.grammar.GrammarParser._
import org.lolczak.dcg.parser.grammar.ast._
import org.lolczak.dcg.parser.language.guard.{GroovyExprEval, ExprEval}

import scala.io.Source
import scalaz.Scalaz._
import scalaz.{-\/, \/, \/-}

object GrammarLoader {

  def load(grammarFile: File): GrammarFailure \/ Grammar =
    for {
      grammarTxt <- loadGrammar(grammarFile)
      loader     =  buildResourceLoader(grammarFile)
      grammar    <- load(grammarTxt, loader)
    } yield grammar

  private def buildResourceLoader(grammarFile: File): ResourceLoader =
    new FileSystemLoader(List(grammarFile.getParent)) + classpathLoader

  private def loadGrammar(grammarFile: File): ResourceLoadFailure \/ String = {
    if (!grammarFile.exists()) -\/(ResourceLoadFailure(s"$grammarFile doesn't exist"))
    else if (grammarFile.isDirectory) -\/(ResourceLoadFailure(s"$grammarFile is directory"))
    else \/-(Source.fromFile(grammarFile, "UTF-8").mkString)
  }

  def load(grammarTxt: String, resourceLoader: ResourceLoader = classpathLoader): GrammarFailure \/ Grammar = {
    for {
      grammarAst       <- parse(grammarTxt)
      importDirectives = grammarAst.directives.collect { case ImportDirective(file) => file }
      imports          <- loadImports(importDirectives, resourceLoader)
      guard            = new GroovyExprEval(imports)
      lexicon          = Lexicon.fromProductions(grammarAst.terminals: _*)
      productions      = processNonterminals(grammarAst.nonterminals, guard)
      nonterminals     = Nonterminals(productions.head.lhs.name, productions)
    } yield Grammar(nonterminals, lexicon)
  }

  private def loadImports(importDirectives: List[String], resourceLoader: ResourceLoader): CodeLoadFailure \/ List[String] = {
    val imports = importDirectives.map { case path =>
      resourceLoader.loadResource(path).toRightDisjunction(path)
    }
    val (errors, contents) = imports.partition(_.isLeft)
    if (errors.isEmpty)
      \/-(contents.collect { case \/-(content) => content})
    else
      -\/(CodeLoadFailure("Cannot load " + errors.collect { case -\/(path) => path} mkString ","))
  }

  private def processNonterminals(nonterminals: List[AstProduction], guard: ExprEval): List[Production] = {
    nonterminals.flatMap { p =>
      val allRhs = permutate(p.rhs)
      val lhs = Term(p.lhs.name, p.lhs.fStruct)
      allRhs.map(rhs => Production(lhs, rhs, p.maybeSnippet.map((_, guard)), p.id))
    }
  }

  private def permutate(rhs: List[RhsSymbol]): List[List[Term]] = {
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

  private def parse(grammarTxt: String): ParsingFailure \/ GrammarAst = {
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