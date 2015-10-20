package org.lolczak.dcg.parser.grammar

import org.lolczak.dcg.model.{FeatureItem, FeatureStruct}

object ast {

  sealed trait Ast

  case class GrammarAst(directives: List[Directive], nonterminals: List[AstProduction], terminals: List[TerminalProduction])

  sealed trait Directive extends Ast
  case class ImportDirective(file: String) extends Directive
  case class IncludeDirective(file: String) extends Directive

  case class AstProduction(lhs: AstTerm, rhs: List[RhsSymbol], maybeSnippet: Option[String] = None, id: Option[String] = None)

  sealed trait RhsSymbol
  case class AstTerm(name: String, fStruct: FeatureStruct = FeatureStruct.empty) extends RhsSymbol
  case class Permutation(terms: List[AstTerm]) extends RhsSymbol

  case class TerminalProduction(lhs: AstTerm, rhs: List[String])

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

    implicit def string2Term(symbol: String): AstTerm = AstTerm(symbol, FeatureStruct.empty)

    implicit class LhsString(term: String) {
      def ~>(rhs: String*): AstProduction = AstProduction(term, List(rhs: _*) map string2Term, None)
    }

    implicit class LhsTerm(term: AstTerm) {
      def ~>(rhs: AstTerm*): AstProduction = AstProduction(term, List(rhs: _*), None)
    }

    implicit class TermString(name: String) {
      def apply(features: (String, FeatureItem)*) = AstTerm(name, FeatureStruct(Map(features: _*)))
    }
  }

}
