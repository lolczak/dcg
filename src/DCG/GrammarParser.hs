module DCG.GrammarParser where

import DCG.Grammar
import Text.Parsec
import qualified Text.Parsec.Token as P
import Control.Monad.Identity
import qualified Data.Map as M

--grammarParser
langDef :: P.GenLanguageDef String () Identity
langDef = P.LanguageDef {
    P.commentStart = "{-",
    P.commentEnd = "-}",
    P.commentLine = "--",
    P.nestedComments = False,
    P.identStart = letter <|> char '_',
    P.identLetter = alphaNum <|> char '_',
    P.opStart = char '-',
    P.opLetter = char '>',
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
       let lexicon = foldl (\acc item -> acc) M.empty lexRules
       return (lexicon, grammar)

rulesParser :: Parsec String () ([LexProduction], [Production])
rulesParser =
    do rules <- manyTill ruleParser $ try eof
       let (ls , ps) = foldl (\(ls, ps) item -> case item of
                                                Left lex -> (lex:ls, ps)
                                                Right prod -> (ls, prod:ps)) ([], []) rules
       return (reverse ls, reverse ps)

ruleParser :: Parsec String () (Either LexProduction Production)
ruleParser = try (do { lexP <- lexerProductionParser; return $ Left lexP }) <|> try  (do { prod <- productionParser; return $ Right prod })

productionParser :: Parsec String () Production
productionParser =
    do whiteSpace
       lhs <- lhsParser <?> "prod lhs"
       rhs <- sepBy1 identifier whiteSpace
       return $ Production (Term lhs) $ map Term rhs

type LexProduction = (String, [String])

lexerProductionParser :: Parsec String () LexProduction
lexerProductionParser =
    do whiteSpace
       lhs <- lhsParser <?> "lexer lhs"
       rhs <- sepBy1 wordParser $ try $ do {whiteSpace; char '|'; whiteSpace}
       return $ (lhs, rhs)

lhsParser :: Parsec String () String
lhsParser =
    do whiteSpace
       lhs <- identifier
       whiteSpace
       string "->"
       whiteSpace
       return lhs

wordParser :: Parsec String () String
wordParser = do char '\''
                word <- many1 letter
                char '\''
                return word

parseGrammar :: String -> Either ParseError (Lexicon, Grammar)
parseGrammar = parse grammarParser ""