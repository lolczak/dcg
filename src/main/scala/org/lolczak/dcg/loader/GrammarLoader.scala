package org.lolczak.dcg.loader

import java.io.File

import org.lolczak.dcg.model._
import org.lolczak.dcg.parser.grammar.GrammarParser
import org.lolczak.dcg.parser.grammar.GrammarParser._
import org.lolczak.dcg.parser.grammar.ast._
import org.lolczak.dcg.parser.language.expr.{GroovyExprEval, ExprEval}

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
      grammarAst        <- parse(grammarTxt)
      includeDirectives =  grammarAst.directives.collect { case IncludeDirective(file) => file }
      modules           <- loadModules(includeDirectives, resourceLoader)
      importDirectives  =  grammarAst.directives.collect { case ImportDirective(file)  => file }
      imports           <- loadImports(importDirectives, resourceLoader)
      guard             =  new GroovyExprEval(imports)
      lexicon           =  Lexicon.fromProductions(grammarAst.terminals: _*)
      productions       =  processNonterminals(grammarAst.nonterminals, guard)
      nonterminals      =  Nonterminals(productions.headOption.map(_.lhs.name).getOrElse(""), productions) //todo refactor it
    } yield combine(Grammar(nonterminals, lexicon), modules)
  }

  private def combine(main: Grammar, modules: List[Grammar]): Grammar = {
    val finalLexicon     = modules.foldLeft(main.lexicon) { case (acc, Grammar(_, lexicon)) => acc + lexicon }
    val finalProductions = modules.foldLeft(main.nonterminals.productions) {
      case (acc, Grammar(Nonterminals(_, productions), _)) => acc ++ productions
    }
    Grammar(Nonterminals(main.nonterminals.start, finalProductions), finalLexicon)
  }

  private def loadModules(importDirectives: List[String], resourceLoader: ResourceLoader): GrammarFailure \/ List[Grammar] = {
    val modules = importDirectives map { path =>
      for {
        content <- resourceLoader.loadResource(path).toRightDisjunction(ResourceLoadFailure(s"Cannot load $path"))
        grammar <- load(content, resourceLoader)
      } yield grammar
    }
    val (errors, grammars) = modules.partition(_.isLeft)
    if (errors.isEmpty)
      \/-(grammars.collect { case \/-(grammar) => grammar})
    else
      -\/(ResourceLoadFailure("Errors occurred: " + errors.collect { case -\/(err) => err}.mkString(","))) //todo refactor it
  }

  private def loadImports(importDirectives: List[String], resourceLoader: ResourceLoader): CodeLoadFailure \/ List[String] = {
    val imports = importDirectives.map { case path => resourceLoader.loadResource(path).toRightDisjunction(path) }
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
      allRhs.map(rhs => Production(lhs, rhs, guard, p.maybeSnippet, p.id))
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