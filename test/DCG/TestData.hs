module DCG.TestData where

import DCG.Grammar
import Util.Container

noun = Term "Noun"
verb = Term "Verb"
det  = Term "Det"
prep = Term "Prep"

lexicon = asMap [ "flies" ~> [noun, verb],
                  "like"  ~> [prep, verb],
                  "time"  ~> [noun],
                  "arrow" ~> [noun],
                  "an"    ~> [det] ]
utterance = ["time", "flies", "like", "an", "arrow"]

grammar = Grammar "S" ["S"  ==> ["NP", "VP"],
                       "VP" ==> ["Verb"],
                       "VP" ==> ["Verb", "NP"],
                       "VP" ==> ["VP", "PP"],
                       "NP" ==> ["Noun"],
                       "NP" ==> ["Det", "Noun"],
                       "NP" ==> ["NP", "PP"],
                       "PP" ==> ["Prep", "NP"]
                       ]

getResource:: FilePath -> (String -> IO()) -> IO()
getResource path f =
    do content <- readFile $ "./resources/" ++ path
       f content