module DCG.TestData where

import DCG.Grammar
import Util.Container

noun = Term "Noun" []
verb = Term "Verb" []
det  = Term "Det" []
prep = Term "Prep" []

lexicon = asMap [ "fly"    ~> [noun, verb],
                  "like"   ~> [prep, verb],
                  "time"   ~> [noun],
                  "arrow"  ~> [noun],
                  "these"  ~> [det],
                  "planes" ~> [noun],
                  "an"     ~> [det] ]

featuredLexicon = asMap [ "fly"    ~> [Term {name = "Noun", fStruct = [("Num",Value "sg")]},Term {name = "Verb", fStruct = []}],
                          "like"   ~> [Term {name = "Prep", fStruct = []},Term {name = "Verb", fStruct = []}],
                          "arrow"  ~> [Term {name = "Noun", fStruct = [("Num",Value "sg")]}],
                          "these"  ~> [Term {name = "Det", fStruct = [("Num",Value "pl")]}],
                          "planes" ~> [Term {name = "Noun", fStruct = [("Num",Value "pl")]}],
                          "an"     ~> [Term {name = "Det", fStruct = [("Num",Value "sg")]}] ]

utterance = ["these", "planes", "fly", "like", "an", "arrow"]

grammar = Grammar "S" ["S"  ==> ["NP", "VP"],
                       "VP" ==> ["Verb"],
                       "VP" ==> ["Verb", "NP"],
                       "VP" ==> ["VP", "PP"],
                       "NP" ==> ["Noun"],
                       "NP" ==> ["Det", "Noun"],
                       "NP" ==> ["NP", "PP"],
                       "PP" ==> ["Prep", "NP"]
                       ]

featureGrammar = Grammar "S" ["S"  ==> ["NP", "VP"],
                              "VP" ==> ["Verb"],
                              "VP" ==> ["Verb", "NP"],
                              "VP" ==> ["VP", "PP"],
                              Production {lhs = Term {name = "NP", fStruct = [("Num",Var "n")]}, rhs = [Term {name = "Noun", fStruct = [("Num",Var "n")]}]},
                              Production {lhs = Term {name = "NP", fStruct = [("Num",Var "n")]}, rhs = [Term {name = "Det", fStruct = [("Num",Var "n")]},Term {name = "Noun", fStruct = [("Num",Var "n")]}]},
                              Production {lhs = Term {name = "NP", fStruct = [("Num",Var "n")]}, rhs = [Term {name = "NP", fStruct = [("Num",Var "n")]},Term {name = "PP", fStruct = []}]},
                              "PP" ==> ["Prep", "NP"]
                              ]

getResource:: FilePath -> (String -> IO()) -> IO()
getResource path f =
    do content <- readFile $ "./resources/" ++ path
       f content