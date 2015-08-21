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
det = Term "Det"
prep = Term "Prep"

spec :: Spec
spec = do
  describe "Chart parser" $ do
    it "should find all terminals" $ do
        let lexicon = asMap [ "flies" ~> [verb, noun],
                              "like"  ~> [prep, verb],
                              "time"  ~> [noun],
                              "arrow" ~> [noun],
                              "an"    ~> [det] ]
        let utterance = ["time", "flies", "like", "an", "arrow"]
        let expected = [asSet [Passive 0 1 $ noun ~~> ["time"]],
                        asSet [Passive 1 2 $ noun ~~> ["flies"], Passive 1 2 $ verb ~~> ["flies"]],
                        asSet [Passive 2 3 $ verb ~~> ["like"], Passive 2 3 $ prep ~~> ["like"]],
                        asSet [Passive 3 4 $ det ~~> ["an"]],
                        asSet [Passive 4 5 $ noun ~~> ["arrow"]]]
        (scan utterance lexicon) `shouldBe` expected