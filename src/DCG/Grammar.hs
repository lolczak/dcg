module DCG.Grammar where

data Grammar = Grammar {start :: String, productions :: [Production]}

data Production = Production Lhs Rhs

data Lhs = Lhs String

data Rhs = Seq [Rhs]
         | Alt Rhs Rhs
         | Lexem String

infix 9 ~>
(~>) :: Lhs -> Rhs -> Production
lhs ~> rhs = Production lhs rhs

validate :: Grammar -> Bool
validate _ = True