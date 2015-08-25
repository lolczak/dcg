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
    P.reservedNames = [],
    P.reservedOpNames = [],
    P.caseSensitive = True
}

lexer       = P.makeTokenParser langDef
whiteSpace  = P.whiteSpace lexer
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer

grammarParser :: Parsec String () (Lexicon, Grammar)
grammarParser =
    do (lexRules, productions) <- rulesParser
       let grammar = Grammar (name $ lhs $ head productions) productions
       let lexicon = foldl (\acc item -> updateLexicon acc item) M.empty lexRules
       return (lexicon, grammar)

updateLexicon :: Lexicon -> LexProduction -> Lexicon
updateLexicon lexicon (term, words) =
    foldl add lexicon words
    where
        add :: Lexicon -> String -> Lexicon
        add l w = case M.lookup w l of
                    Just ts -> M.insert w (Term term : ts) l
                    Nothing -> M.insert w [Term term] l

rulesParser :: Parsec String () ([LexProduction], [Production])
rulesParser =
    do rules <- manyTill ruleParser $ try eof
       let (ls , ps) = foldl (\(ls, ps) item -> case item of
                                                Left lex -> (lex:ls, ps)
                                                Right prod -> (ls, prod:ps)) ([], []) rules
       return (reverse ls, reverse ps)

ruleParser :: Parsec String () (Either LexProduction Production)
ruleParser = try (do { lexP <- terminal; return $ Left lexP }) <|> (do { prod <- nonterminal; return $ Right prod })

nonterminal :: Parsec String () Production
nonterminal =
    do whiteSpace
       lhs <- lhsParser <?> "prod lhs"
       whiteSpace
       rhs <- sep (termId <?> "rhs term") termSeparator ruleEnd
       return $ Production (Term lhs) $ map Term rhs

terminal :: Parsec String () LexProduction
terminal =
    do whiteSpace
       lhs <- lhsParser <?> "lexer lhs"
       whiteSpace
       rhs <- sep wordParser (do {whiteSpace; char '|'; whiteSpace}) ruleEnd
       return $ (lhs, rhs)

sep :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m end -> ParsecT s u m [a]
sep p s end =
    do x <- p
       xs <- manyTill (s >> p) $ try end
       return (x:xs)

ruleEnd :: Parsec String () ()
ruleEnd = do whiteSpace
             --(try eof <|> (endOfLine >> return ()))
             try eof <|> (lookAhead lhsParser >> return ())
             return ()

term :: Parsec String () Term
term =
    do id <- termId
       return $ Term id

termId :: Parsec String () String
termId = identifier

termSeparator :: Parsec String () ()
termSeparator = whiteSpace

lhsParser :: Parsec String () String
lhsParser =
    do whiteSpace
       lhs <- termId
       whiteSpace
       string "->"
       return lhs

wordParser :: Parsec String () String
wordParser = do char '\''
                word <- many1 letter
                char '\''
                return word

parseGrammar :: String -> Either ParseError (Lexicon, Grammar)
parseGrammar = parse grammarParser ""