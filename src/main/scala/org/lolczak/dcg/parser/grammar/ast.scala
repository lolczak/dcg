package org.lolczak.dcg.parser.grammar

import org.lolczak.dcg.model.{FeatureItem, FeatureStruct}

object ast {

  sealed trait Ast

  case class GrammarAst(directives: List[Directive], nonterminals: List[Production], terminals: List[TerminalProduction])

  sealed trait Directive extends Ast
  case class ImportDirective(file: String) extends Directive
  case class IncludeDirective(file: String) extends Directive

  case class Production(lhs: Term, rhs: List[RhsSymbol], maybeSnippet: Option[String] = None, id: Option[String] = None)

  sealed trait RhsSymbol
  case class Term(name: String, fStruct: FeatureStruct = FeatureStruct.empty) extends RhsSymbol
  case class Permutation(terms: List[Term]) extends RhsSymbol

  case class TerminalProduction(lhs: Term, rhs: List[String])

  //  case class Optional
  /*
  Term[all@_]
  Term[all@(num="pl", case="nom")]
  Term[num="pl", case=a@<"pl", "sg">]
  Term[num="pl", case=a@[a="pl", b="sg"]]
  sealed trait FeatureDef
  case class Binding[A](name:String, expr: A) extends FeatureDef
   */

  object ops {

    implicit def string2Term(symbol: String): Term = Term(symbol, FeatureStruct.empty)

    implicit class LhsString(term: String) {
      def ~>(rhs: String*): Production = Production(term, List(rhs: _*) map string2Term, None)
    }

    implicit class LhsTerm(term: Term) {
      def ~>(rhs: Term*): Production = Production(term, List(rhs: _*), None)
    }

    implicit class TermString(name: String) {
      def apply(features: (String, FeatureItem)*) = Term(name, FeatureStruct(Map(features: _*)))
    }
  }

}
