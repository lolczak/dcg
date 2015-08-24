module DCG.ChartParserSpec where

import Test.Hspec
import DCG.Grammar
import DCG.ChartParser
import Util.Container

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

noun = Term "Noun"
verb = Term "Verb"
det  = Term "Det"
prep = Term "Prep"

lexicon = asMap [ "flies" ~> [verb, noun],
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

    it "should parse new active rules" $ do
        parse lexicon grammar utterance `shouldBe` [Node {t = Term "S", children = [Node {t = Term "NP", children = [Node {t = Term "Noun", children = [Leaf "time"]}]},Node {t = Term "VP", children = [Node {t = Term "VP", children = [Node {t = Term "Verb", children = [Leaf "flies"]}]},Node {t = Term "PP", children = [Node {t = Term "Prep", children = [Leaf "like"]},Node {t = Term "NP", children = [Node {t = Term "Det", children = [Leaf "an"]},Node {t = Term "Noun", children = [Leaf "arrow"]}]}]}]}]}]
