module DCG.ChartParser where

import DCG.Grammar
import Util.Container
import qualified Data.Set as Set

data Edge = Passive {start :: Int, end :: Int, found :: Term, tree :: ParseTree Term String}
          | Active  {start :: Int, end :: Int, found :: Term, remaining :: [Term], pchildren :: [ParseTree Term String]}
          deriving (Eq, Ord, Show)


type State = Set.Set Edge

type Chart = [State]

data ParseTree t l = Leaf l | Node { t :: Term, children :: [ParseTree t l] } deriving (Eq, Ord, Show)

buildChart :: [String] -> Grammar -> [Edge]
buildChart = error "Not implemented"

scan :: [String] -> Lexicon -> Chart
scan ts l =
    map findTerminal $ zip [0..] ts
    where findTerminal :: (Int, String) -> State
          findTerminal (index, w) = case findInLexicon l w of
                                    Just xs -> asSet $ map (\x -> Passive index (index+1) x $ Leaf w) xs
                                    Nothing -> Set.empty

predict :: Grammar -> State -> State
predict grammar oldState =
    asSet $ [ Active s e ruleTerm tail [tree]|
             Production ruleTerm (prefix : tail) <- productions grammar,
             Passive s e edgeTerm tree           <- Set.elems oldState, prefix == edgeTerm ]
             ++ Set.elems oldState

--combine