module DCG.GrammarParserSpec where

import Test.Hspec
import DCG.Grammar
import DCG.ChartParser
import Util.Container
import DCG.GrammarParser
import Text.Parsec
import qualified Data.Map as M
import DCG.TestData

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Grammar parser" $ do
    context "for nonterminal productions" $ do
        it "should parse rhs containing one constituent" $ do
            parse nonterminal "" "S -> NP \n" `shouldBe` Right (Production (Term "S" []) [Term "NP" []])

        it "should parse rhs ending on new line" $ do
            parse nonterminal "" "S -> NP VP \n" `shouldBe` Right (Production (Term "S" []) [Term "NP" [], Term "VP" []])

        it "should parse rhs ending on eof" $ do
            parse nonterminal "" "S -> NP VP Det" `shouldBe` Right (Production (Term "S" []) [Term "NP" [], Term "VP" [], Term "Det" []])

    context "for terminal productions" $ do
        it "should parse rhs containing one constituent" $ do
            parse terminal "" "Verb -> 'like'\n" `shouldBe` Right (Term "Verb" [], ["like"])

        it "should parse rhs ending on new line" $ do
            parse terminal "" "Verb -> 'like' | 'test' \n" `shouldBe` Right (Term "Verb" [], ["like", "test"])

    context "for rule end" $ do
        it "should match new line" $ do
            parse productionEnd "" "\n" `shouldBe` Right ()

        it "should match new line with one space" $ do
            parse productionEnd "" " \n" `shouldBe` Right ()

        it "should match new line with many spaces" $ do
            parse productionEnd "" "   \n" `shouldBe` Right ()

    it "should parse whole grammar" $ getResource "simple_gram.dcg" $ \grammarString -> do
        parseGrammar grammarString `shouldBe` Right (lexicon, grammar)
