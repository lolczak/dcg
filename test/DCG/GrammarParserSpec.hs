module DCG.GrammarParserSpec where

import Test.Hspec
import DCG.Grammar
import DCG.ChartParser
import Util.Container
import DCG.GrammarParser

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

gramarString = "\
\S -> NP VP \
\VP -> Verb \
\VP -> Verb, NP \
\VP -> VP PP \
\NP -> Noun \
\NP -> Det Noun \
\NP -> NP PP \
\PP -> Prep NP' \
\Verb -> 'flies' | 'like' \
\Noun -> 'flies' | 'time' | 'arrow' \
\Det -> 'an' \
\Prep -> 'like' \
\"

spec :: Spec
spec = do
  describe "Grammar parser" $ do
    it "should parse grammar" $ do
            parseGrammar gramarString `shouldBe` (lexicon, grammar)
