module Util.Container where

import qualified Data.Set as Set
import qualified Data.Map as Map

infix 9 ~>
(~>) :: a -> b -> (a, b)
a ~> b = (a, b)

asSet :: Ord a => [a] -> Set.Set a
asSet = foldl (flip Set.insert) Set.empty

asMap :: Ord k => [(k,e)] -> Map.Map k e
asMap = foldl (flip $ uncurry Map.insert) Map.empty