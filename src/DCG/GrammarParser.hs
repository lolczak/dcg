module DCG.GrammarParser where

import DCG.Grammar
import Text.Parsec
import qualified Text.Parsec.Token as P
import Control.Monad.Identity
import qualified Data.Map as M

type LexProduction = (String, [String])

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
                    Just ts -> M.insert w (Term term : ts) l
                    Nothing -> M.insert w [Term term] l

allProductions :: Parsec String () ([LexProduction], [Production])
allProductions =
    do rules <- manyTill production $ try eof
       let (ls , ps) = foldl (\(ls, ps) item -> case item of
                                                Left lex -> (lex:ls, ps)
                                                Right prod -> (ls, prod:ps)) ([], []) rules
       return (reverse ls, reverse ps)

production :: Parsec String () (Either LexProduction Production)
production = try (do { prod <- terminal; return $ Left prod }) <|> (do { prod <- nonterminal; return $ Right prod })

nonterminal :: Parsec String () Production
nonterminal =
    do whiteSpace
       lhs <- productionLhs <?> "prod lhs"
       whiteSpace
       rhs <- separatedSequence (termId <?> "rhs term") termSeparator productionEnd
       return $ Production (Term lhs) $ map Term rhs

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
       return $ Term id

termId :: Parsec String () String
termId = identifier

termSeparator :: Parsec String () ()
termSeparator = whiteSpace

productionLhs :: Parsec String () String
productionLhs =
    do whiteSpace
       lhs <- termId
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
