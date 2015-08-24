module DCG.ChartParser where

import DCG.Grammar
import Util.Container
import qualified Data.Set as S

data Edge = Passive {start :: Int, end :: Int, found :: Term, tree :: ParseTree Term String}
          | Active  {start :: Int, end :: Int, found :: Term, remaining :: [Term], pchildren :: [ParseTree Term String]}
          deriving (Eq, Ord)

instance Show Edge where
    show (Passive s e f t) = "(" ++ show s ++ "," ++ show e ++ "," ++ show f ++ ")"
    show (Active  s e f r cs) = "(" ++ show s ++ "," ++ show e ++ "," ++ show f ++ (foldl (\x y -> x ++ show y ++ ",") ", { " r) ++ "} )"

type State = S.Set Edge

type Chart = [State]

data ParseTree t l = Leaf l | Node { t :: Term, children :: [ParseTree t l] } deriving (Eq, Ord)

instance (Show t, Show l) => Show (ParseTree t l) where
    show parseTree = "\n" ++ render 0 parseTree

render :: (Show t, Show l) => Int -> ParseTree t l -> String
render indent (Leaf l) = (replicate indent '\t') ++ "'" ++ show l ++ "'\n"
render indent (Node t nodes) = (replicate indent '\t') ++ show t ++ "\n" ++ (foldl (\x y -> x ++ render (indent+1) y) "" nodes)

parse :: Lexicon -> Grammar -> [String] -> [ParseTree Term String]
parse l g u =
    [ parseTree | Passive 0 _ found parseTree <- S.elems $ last chart, found == (Term $ topTerm g)]
    where chart = buildChart l g u

buildChart :: Lexicon -> Grammar -> [String] -> Chart
buildChart l g u =
    foldl buildStates [] $ zip [0..] u
    where
        buildStates :: Chart -> (Int, String) -> Chart
        buildStates old (index, word) = old ++ [newState]
            where
                afterScan = scan word index l
                newState = generate (\x -> S.union (predict g x) (combine old x)) afterScan

generate :: (Edge -> State) -> State -> State
generate f oldState =
    recGen oldState oldState
    where recGen old new
            | S.null generated = old
            | otherwise = recGen (S.union generated old) (S.difference generated old)
            where generated = S.unions $ map f (S.elems new)

scan :: String -> Int -> Lexicon -> State
scan w i l =  case findInLexicon l w of
                   Just xs -> asSet $ map (\x -> Passive i (i+1) x $ Node x [Leaf w]) xs
                   Nothing -> S.empty

predict :: Grammar -> Edge -> State
predict grammar (Passive s e edgeTerm parseTree) =
    asSet $ [ if null rest
              then Passive s e ruleTerm (Node ruleTerm [parseTree])
              else Active s e ruleTerm rest [parseTree] |
             Production ruleTerm (prefix : rest) <- productions grammar,
             prefix == edgeTerm ]
predict _ _ = S.empty

combine :: Chart -> Edge -> State
combine chart (Passive j k rightTerm parseTree) =
    asSet $ [ if null rest then Passive i k leftTerm (Node leftTerm (reverse $ parseTree : cs)) else Active i k leftTerm rest (parseTree:cs) |
            Active i j' leftTerm (prefix:rest) cs <- if j == 0 then [] else S.elems $ chart !! (j-1),
            prefix == rightTerm && j == j']
combine _ _ = S.empty
