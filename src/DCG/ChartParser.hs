module DCG.ChartParser where

import DCG.Grammar
import Util.Container
import qualified Data.Set as Set

data Edge = Passive {start :: Int, end :: Int, found :: Term, tree :: ParseTree Term String}
          | Active  {start :: Int, end :: Int, found :: Term, remaining :: [Term], pchildren :: [ParseTree Term String]}
          deriving (Eq, Ord)

instance Show Edge where
    show (Passive s e f t) = "(" ++ show s ++ "," ++ show e ++ "," ++ show f ++ ")"
    show (Active  s e f r cs) = "(" ++ show s ++ "," ++ show e ++ "," ++ show f ++ (foldl (\x y -> x ++ show y ++ ",") ", { " r) ++ "} )"

type State = Set.Set Edge

type Chart = [State]

data ParseTree t l = Leaf l | Node { t :: Term, children :: [ParseTree t l] } deriving (Eq, Ord)

instance (Show t, Show l) => Show (ParseTree t l) where
    show parseTree = "\n" ++ render 0 parseTree

render :: (Show t, Show l) => Int -> ParseTree t l -> String
render indent (Leaf l) = (replicate indent '\t') ++ "'" ++ show l ++ "'\n"
render indent (Node t nodes) = (replicate indent '\t') ++ show t ++ "\n" ++ (foldl (\x y -> x ++ render (indent+1) y) "" nodes)

parse :: Lexicon -> Grammar -> [String] -> [ParseTree Term String]
parse l g u =
    [ parseTree | Passive 0 _ found parseTree <- Set.elems $ last chart, found == (Term $ topTerm g)]
    where chart = buildChart l g u

buildChart :: Lexicon -> Grammar -> [String] -> Chart
buildChart l g u =
    foldl buildStates [] $ zip [0..] u
    where
        buildStates :: Chart -> (Int, String) -> Chart
        buildStates old (index, word) = foldl (\chart i -> combine i chart) afterCombination $ [0..index]++[0..index]++[0..index]
            where
                afterScan = old ++ [(scan' word index l)]
                afterPrediction = map (predict g) afterScan
                afterCombination = foldl (\chart i -> combine i chart) afterPrediction [0..index]

scan' :: String -> Int -> Lexicon -> State
scan' w i l =  case findInLexicon l w of
                   Just xs -> asSet $ map (\x -> Passive i (i+1) x $ Node x [Leaf w]) xs
                   Nothing -> Set.empty

scan :: [String] -> Lexicon -> Chart
scan ts l =
    map findTerminal $ zip [0..] ts
    where findTerminal :: (Int, String) -> State
          findTerminal (index, w) = case findInLexicon l w of
                                    Just xs -> asSet $ map (\x -> Passive index (index+1) x $ Node x [Leaf w]) xs
                                    Nothing -> Set.empty

predict :: Grammar -> State -> State
predict grammar oldState =
    asSet $ [ if null rest then Passive s e ruleTerm (Node ruleTerm [tree]) else Active s e ruleTerm rest [tree]|
             Production ruleTerm (prefix : rest) <- productions grammar,
             Passive s e edgeTerm tree           <- Set.elems oldState, prefix == edgeTerm ]
             ++ Set.elems oldState

-- Chart -> State->State!!
combine :: Int -> Chart -> Chart
combine index chart =
    update index new chart
    where
    new = asSet $ [ if null rest then Passive i k leftTerm (Node leftTerm (reverse $ parseTree : cs)) else Active i k leftTerm rest (parseTree:cs) |
                    Passive j k rightTerm parseTree <- Set.elems $ chart !! index,
                    Active i j' leftTerm (prefix:rest) cs <- if j == 0 then [] else Set.elems $ chart !! (j-1),
                    prefix == rightTerm && j == j']
                   ++ (Set.elems $ chart !! index)


update :: Int -> a -> [a] -> [a]
update i e xs
    | i == 0 = e : tail xs
    | otherwise = take (i) xs ++ [e] ++ drop (i+1) xs
