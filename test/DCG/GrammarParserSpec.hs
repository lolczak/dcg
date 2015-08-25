module DCG.GrammarParserSpec where

import Test.Hspec
import DCG.Grammar
import DCG.ChartParser
import Util.Container
import DCG.GrammarParser
import Text.Parsec
import qualified Data.Map as M

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

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

grammarString = "-- comment \n\
\S -> NP VP {- fwefwfwefwf -}\n\
\VP -> Verb\n\
\VP -> Verb NP   \n\
\VP -> VP PP \n\
\NP -> Noun \n\
\NP -> Det Noun \n\
\NP -> NP PP \n\
\PP -> Prep NP\n\
\Verb -> 'flies' | 'like' \n\
\Noun -> 'flies' | 'time' | 'arrow' \n\
\Det -> 'an' \n\
\Prep -> 'like' \n\
\"

spec :: Spec
spec = do
  describe "Grammar parser" $ do
    context "for nonterminal productions" $ do
        it "should parse rhs containing one constituent" $ do
            parse nonterminal "" "S -> NP \n" `shouldBe` Right (Production (Term "S") [Term "NP"])

        it "should parse rhs ending on new line" $ do
            parse nonterminal "" "S -> NP VP \n" `shouldBe` Right (Production (Term "S") [Term "NP", Term "VP"])

        it "should parse rhs ending on eof" $ do
            parse nonterminal "" "S -> NP VP Det" `shouldBe` Right (Production (Term "S") [Term "NP", Term "VP", Term "Det"])

    context "for terminal productions" $ do
        it "should parse rhs containing one constituent" $ do
            parse terminal "" "Verb -> 'like'\n" `shouldBe` Right (Term "Verb", ["like"])

        it "should parse rhs ending on new line" $ do
            parse terminal "" "Verb -> 'like' | 'test' \n" `shouldBe` Right (Term "Verb", ["like", "test"])

    context "for rule end" $ do
        it "should match new line" $ do
            parse productionEnd "" "\n" `shouldBe` Right ()

        it "should match new line with one space" $ do
            parse productionEnd "" " \n" `shouldBe` Right ()

        it "should match new line with many spaces" $ do
            parse productionEnd "" "   \n" `shouldBe` Right ()

    it "should parse whole grammar" $ do
        parseGrammar grammarString `shouldBe` Right (lexicon, grammar)

--    it "should parse whole grammar2" $ do
--        let gram = "S -> NP VP \n T -> Verb R \nV -> Verb NP"
--        parseGrammar gram `shouldBe` Right (M.empty, grammar)
