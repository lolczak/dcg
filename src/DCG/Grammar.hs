module DCG.Grammar where

import qualified Data.Map as Map

data Grammar = Grammar {topTerm :: String, productions :: [Production]} deriving (Eq, Show)

data Production = Production {lhs :: Term, rhs :: [Term]}
--                | Terminal Term [String]
                deriving (Eq, Ord, Show)

data Term = Term {name :: String, fStruct :: AVM} deriving (Eq, Ord, Show)

data FValue = Value String | Var String | List [String]  deriving (Eq, Ord, Show)
type Feature = (String, FValue)
type AVM = [Feature]

--data Rhs = Seq deriving (Eq, Ord, Show)

type Lexicon = Map.Map String [Term]

findInLexicon :: Lexicon -> String -> Maybe [Term]
findInLexicon l w = Map.lookup w l

infix 9 ==>
(==>) :: String -> [String] -> Production
lhs ==> rhs = Production (Term lhs []) $ map (\x -> Term x []) rhs

infix 9 ~~>
(~~>) :: Term -> [String] -> Lexicon
lhs ~~> rhs = error "Not implemented"

validate :: Grammar -> Bool
validate _ = True