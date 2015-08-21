module DCG.Grammar where

import qualified Data.Map as Map

data Grammar = Grammar {start :: String, productions :: [Production]}

data Production = NonTerminal Term Rhs
                | Terminal Term [String]
                deriving (Eq, Ord, Show)

data Term = Term String deriving (Eq, Ord, Show)

data Rhs = Seq [Term] deriving (Eq, Ord, Show)

type Lexicon = Map.Map String [Term]

findInLexicon :: Lexicon -> String -> Maybe [Term]
findInLexicon l w = Map.lookup w l

infix 9 ==>
(==>) :: Term -> Rhs -> Production
lhs ==> rhs = NonTerminal lhs rhs

infix 9 ~~>
(~~>) :: Term -> [String] -> Production
lhs ~~> rhs = Terminal lhs rhs

validate :: Grammar -> Bool
validate _ = True