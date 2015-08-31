package org.lolczak.dcg.grammar

import org.lolczak.dcg._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object GrammarParser extends StandardTokenParsers {
  lexical.reserved ++= List("->")
  lexical.delimiters ++= List("{", "}", "[", "]", "=", ",", "?")

  lazy val term: Parser[Term] =
    for {
      symbolName <- ident
      maybeFStruct <- opt(featureStruct)
    } yield Term(symbolName, maybeFStruct.getOrElse(FeatureStruct.empty))

  lazy val featureStruct: Parser[FeatureStruct] =
    "[" ~> separatedSequence(feature, featureSeparator, featureEnd) ^^ { f => FeatureStruct(Map(f: _*)) }

  lazy val feature: Parser[(String, FValue)] = (ident <~ "=") ~ (fvariable | fvalue) ^^ { case name ~ fval => (name, fval) }

  lazy val fvariable: Parser[FVariable] = "?" ~> ident ^^ { varName => FVariable(varName) }

  lazy val fvalue: Parser[FConst] = ident ^^ { varName => FConst(varName) }

  lazy val featureSeparator: Parser[Unit] = "," ^^ { _ => () }

  lazy val featureEnd: Parser[Unit] = "]" ^^ { _ => () }

  //  def char(char: Char): Parser[Char] = elem("", _.chars == char.toString) ^^ { _ => char }

  def repTill[T](p: => Parser[T], end: => Parser[Any]): Parser[List[T]] =
    end ^^ { _ => List.empty } | (p ~ repTill(p, end)) ^^ { case x ~ xs => x :: xs }

  def separatedSequence[T](p: => Parser[T], s: => Parser[Any], end: => Parser[Any]): Parser[List[T]] =
    for {
      x <- p
      xs <- repTill(s ~> p, end)
    } yield x :: xs

  /*

feature :: Parsec String () Feature
feature =
    do whiteSpace
       featureName <- identifier
       whiteSpace
       char '='
       whiteSpace
       varName <- choice [fvariable, fvalue]
       return (featureName, varName)

fvalue :: Parsec String () FValue
fvalue = do val <- identifier
            return $ Value val

fvariable :: Parsec String () FValue
fvariable =
    do char '?'
       varName <- identifier
       return $ Var varName

featureSeparator :: Parsec String () ()
featureSeparator = whiteSpace >> char ',' >> whiteSpace

featureEnd :: Parsec String () ()
featureEnd = whiteSpace >> char ']' >> return ()
   */

}

/*
module DCG.GrammarParser where

import DCG.Grammar
import Text.Parsec
import qualified Text.Parsec.Token as P
import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.Foldable as F

type LexProduction = (Term, [String])

--grammarParser
langDef :: P.GenLanguageDef String () Identity
langDef = P.LanguageDef {
    P.commentStart = "{-",
    P.commentEnd = "-}",
    P.commentLine = "--",
    P.nestedComments = False,
    P.identStart = letter,
    P.identLetter = letter,
    P.opStart = char '#',
    P.opLetter = char '$',
    P.reservedNames = ["->"],
    P.reservedOpNames = ["->"],
    P.caseSensitive = True
}

lexer       = P.makeTokenParser langDef
whiteSpace  = P.whiteSpace lexer
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer

parseGrammar :: String -> Either ParseError (Lexicon, Grammar)
parseGrammar = parse grammarParser ""

grammarParser :: Parsec String () (Lexicon, Grammar)
grammarParser =
    do (terminals, nonterminals) <- allProductions
       let grammar = Grammar (name $ lhs $ head nonterminals) nonterminals
       let lexicon = foldl (\acc item -> updateLexicon acc item) M.empty terminals
       return (lexicon, grammar)

updateLexicon :: Lexicon -> LexProduction -> Lexicon
updateLexicon lexicon (term, words) =
    foldl add lexicon words
    where
        add :: Lexicon -> String -> Lexicon
        add l w = case M.lookup w l of
                    Just ts -> M.insert w (term : ts) l
                    Nothing -> M.insert w [term] l

allProductions :: Parsec String () ([LexProduction], [Production])
allProductions =
    do rules <- manyTill production $ try eof
       let (ls , ps) = foldl (\(ls, ps) item -> case item of
                                                Left lex -> (lex:ls, ps)
                                                Right prod -> (ls, prod:ps)) ([], []) rules
       return (reverse ls, reverse ps)

production :: Parsec String () (Either LexProduction Production)
production = try (fmap Left terminal) <|> (fmap Right nonterminal)

nonterminal :: Parsec String () Production
nonterminal =
    do whiteSpace
       lhs <- productionLhs <?> "prod lhs"
       whiteSpace
       rhs <- separatedSequence (term <?> "rhs term") termSeparator productionEnd
       return $ Production lhs rhs

terminal :: Parsec String () LexProduction
terminal =
    do whiteSpace
       lhs <- productionLhs <?> "lexer lhs"
       whiteSpace
       rhs <- separatedSequence word wordSeparator productionEnd
       return $ (lhs, rhs)

separatedSequence :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m end -> ParsecT s u m [a]
separatedSequence p s end =
    do x <- p
       xs <- manyTill (s >> p) $ try end
       return (x:xs)

productionEnd :: Parsec String () ()
productionEnd =
    do whiteSpace
       try eof <|> (lookAhead productionLhs >> return ())
       return ()

term :: Parsec String () Term
term =
    do id <- termId
       fStruct <- optionMaybe avm
       return $ Term id $ F.foldl (\list x -> x ++ list) [] fStruct

termId :: Parsec String () String
termId = identifier

avm :: Parsec String () [Feature]
avm =
    do whiteSpace
       char '['
       whiteSpace
       features <- separatedSequence feature featureSeparator featureEnd
       return features

feature :: Parsec String () Feature
feature =
    do whiteSpace
       featureName <- identifier
       whiteSpace
       char '='
       whiteSpace
       varName <- choice [fvariable, fvalue]
       return (featureName, varName)

fvalue :: Parsec String () FValue
fvalue = do val <- identifier
            return $ Value val

fvariable :: Parsec String () FValue
fvariable =
    do char '?'
       varName <- identifier
       return $ Var varName

featureSeparator :: Parsec String () ()
featureSeparator = whiteSpace >> char ',' >> whiteSpace

featureEnd :: Parsec String () ()
featureEnd = whiteSpace >> char ']' >> return ()

termSeparator :: Parsec String () ()
termSeparator = whiteSpace

productionLhs :: Parsec String () Term
productionLhs =
    do whiteSpace
       lhs <- term
       whiteSpace
       string "->"
       return lhs

wordSeparator :: Parsec String () ()
wordSeparator = whiteSpace >> char '|' >> whiteSpace

word :: Parsec String () String
word =
    do char '\''
       word <- many1 letter
       char '\''
       return word


 */