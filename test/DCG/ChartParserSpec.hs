module DCG.ChartParserSpec where

import Test.Hspec
import DCG.Grammar
import DCG.ChartParser
import Util.Container
import DCG.TestData

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Chart parser" $ do
--    it "should find all terminals" $ do
--        let expected = [asSet [Passive 0 1 noun $ Leaf "time"],
--                        asSet [Passive 1 2 noun $ Leaf "flies", Passive 1 2 verb $ Leaf "flies"],
--                        asSet [Passive 2 3 verb $ Leaf "like",  Passive 2 3 prep $ Leaf "like"],
--                        asSet [Passive 3 4 det  $ Leaf "an"],
--                        asSet [Passive 4 5 noun $ Leaf "arrow"]]
--        (scan utterance lexicon) `shouldBe` expected
--
--    it "should predict new active rules" $ do
--            let expected = [asSet [Passive 0 1 noun $ Leaf "time"],
--                            asSet [Passive 1 2 noun $ Leaf "flies", Passive 1 2 verb $ Leaf "flies"],
--                            asSet [Passive 2 3 verb $ Leaf "like",  Passive 2 3 prep $ Leaf "like"],
--                            asSet [Passive 3 4 det  $ Leaf "an"],
--                            asSet [Passive 4 5 noun $ Leaf "arrow"]]
--            let chart = scan utterance lexicon
--            (map (predict grammar) chart) `shouldBe` expected

    context "variables in production" $ do
        it "should return true for bindable rule" $ do
            let prod = Production {lhs = Term {name = "NP", fStruct = [("Num",Var "n")]}, rhs = [Term {name = "Det", fStruct = [("Num",Var "n")]},Term {name = "Noun", fStruct = [("Num",Var "n")]}]}
            let parseTree = [Node (Term "Det" [("Num",Value "pl")]) [], Node (Term "Noun" [("Num", Value "pl")]) []]
            isBindable prod parseTree `shouldBe` True

        it "should return false for unbindable rule" $ do
            let prod = Production {lhs = Term {name = "NP", fStruct = [("Num",Var "n")]}, rhs = [Term {name = "Det", fStruct = [("Num",Var "n")]},Term {name = "Noun", fStruct = [("Num",Var "n")]}]}
            let parseTree = [Node (Term "Det" [("Num", Value "pl")]) [], Node (Term "Noun" [("Num", Value "sg")]) []]
            isBindable prod parseTree `shouldBe` False

        it "should return false for missing value" $ do
            let prod = Production {lhs = Term {name = "NP", fStruct = [("Num",Var "n")]}, rhs = [Term {name = "Det", fStruct = [("Num",Var "n")]},Term {name = "Noun", fStruct = [("Num",Var "n")]}]}
            let parseTree = [Node (Term "Det" [("Num", Value "pl")]) [], Node (Term "Noun" []) []]
            isBindable prod parseTree `shouldBe` False

        it "should bind variables in production" $ do
            let prod = Production {lhs = Term {name = "NP", fStruct = [("Num",Var "n")]}, rhs = [Term {name = "Det", fStruct = [("Num",Var "n")]},Term {name = "Noun", fStruct = [("Num",Var "n")]}]}
            let parseTree = [Node (Term "Det" [("Num",Value "pl")]) [], Node (Term "Noun" [("Num", Value "pl")]) []]
            bindVar prod parseTree `shouldBe` Term "NP" [("Num", Value "pl")]

    it "should parse correct utterance" $ do
        parseGCD lexicon grammar utterance `shouldBe` [Node {t = Term "S" [], children = [Node {t = Term "NP" [], children = [Node {t = Term "Det" [], children = [Leaf "these"]},Node {t = Term "Noun" [], children = [Leaf "planes"]}]},Node {t = Term "VP" [], children = [Node {t = Term "VP" [], children = [Node {t = Term "Verb" [], children = [Leaf "fly"]}]},Node {t = Term "PP" [], children = [Node {t = Term "Prep" [], children = [Leaf "like"]},Node {t = Term "NP" [], children = [Node {t = Term "Det" [], children = [Leaf "an"]},Node {t = Term "Noun" [], children = [Leaf "arrow"]}]}]}]}]}]
