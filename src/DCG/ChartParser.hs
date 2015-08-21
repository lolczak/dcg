module DCG.ChartParser where

import DCG.Grammar
import Util.Container
import qualified Data.Set as Set

data Edge = Passive {start :: Int, end :: Int, rule :: Production}
          | Active  {start :: Int, end :: Int, rule :: Production, remaining :: [Production]}
          deriving (Eq, Ord, Show)


type State = Set.Set Edge

type Chart = [State]

buildChart :: [String] -> Grammar -> [Edge]
buildChart = error "Not implemented"

scan :: [String] -> Lexicon -> Chart
scan ts l =
    map findTerminal $ zip [0..] ts
    where findTerminal :: (Int, String) -> State
          findTerminal (index, w) = case findInLexicon l w of
                                    Just xs -> asSet $ map (\x -> Passive index (index+1) $ Terminal x [w]) xs
                                    Nothing -> Set.empty




--predict

--combine