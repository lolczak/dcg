module DCG.ChartParser where

import DCG.Grammar
import qualified Data.Set as Set

data Edge = Passive {start :: Int, end :: Int, rule :: Production}
          | Active  {start :: Int, end :: Int, rule :: Production, remaining :: [Production]}


type State = Set.Set Edge

type Chart = [State]

buildChart :: [String] -> Grammar -> [Edge]
buildChart = fail "sdfsdf"

scan :: [String] -> Lexicon -> Chart
scan ts l =
    map findTerminal ts
    where findTerminal w = case findInLexicon l w of
                                Just xs -> 
                                Nothing -> Set.empty


--predict

--combine