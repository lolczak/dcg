module DCG.Grammar where

data Grammar = Grammar {start :: String, productions :: [Production]}

data Production = Production Lhs Rhs

data Lhs = Lhs String

data Rhs = Seq [Rhs]
         | Alt Rhs Rhs
         | Lexem String


validate :: Grammar -> Bool
validate _ = True