package org.lolczak.dcg

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.lolczak.dcg.model.Term
import org.lolczak.dcg.parser.grammar.GrammarParser
import org.lolczak.dcg.parser.language.{Leaf, Node, ChartParser, ParseTree}

import scala.io.Source

object DcgApp extends App with LazyLogging {

  val grammarPath = args(0)

  if (args.length != 2) {
    printHelp()
    System.exit(1)
  }
  val utterance = args(1)
  logger.info(s"Parsing grammar $grammarPath")
  val grammar = GrammarParser.parseGrammar(Source.fromFile(new File(grammarPath), "UTF-8").mkString).get
  logger.info(s"Parsing language... ")
  val parser = new ChartParser(grammar)
  val results = parser.parse(utterance)

  def printHelp() = {
    println("Usage: dcg <grammar_path> <utterance>")
  }

  render(results)

  def render(results: List[ParseTree[Term, String]]): Unit = {
    println(s"Found ${results.size} results.")
    (results zip Stream.from(0)).foreach { case (r, idx) =>
      println(s"Result no $idx:")
      render(r)
    }
  }

  def render(root: ParseTree[Term, String]): Unit = {
    def renderNode(node: ParseTree[Term, String], depth:Int=0): Unit = {
      node match {
        case Leaf(symbol) => printIndentation(depth); println(s"Leaf '$symbol'")
        case Node(term, children, prodId) => printIndentation(depth); println(s"Node $term ($prodId)"); children.foreach(renderNode(_, depth+1))
      }
    }
    renderNode(root, 0)
  }

  def printIndentation(depth: Int): Unit = {
    if (depth <= 0) ()
    else (1 to depth) foreach (_ => print("  "))
  }


}
