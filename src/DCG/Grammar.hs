module DCG.Grammar where

import qualified Data.Map as Map

data Grammar = Grammar {start :: String, productions :: [Production]}

data Production = Production Lhs Rhs

data Lhs = Lhs String

data Rhs = Seq [Rhs]
         | Alt Rhs Rhs

data LexProduction = LexProduction Lhs LexRhs

type LexRhs = [String]

type Lexicon = Map.Map String [Lhs]

findInLexicon :: Lexicon -> String -> Maybe [Lhs]
findInLexicon l w = Map.lookup w l

infix 9 ~>
(~>) :: Lhs -> Rhs -> Production
lhs ~> rhs = Production lhs rhs

validate :: Grammar -> Bool
validate _ = True