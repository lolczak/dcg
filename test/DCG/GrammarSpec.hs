module DCG.GrammarSpec where

import Test.Hspec
import DCG.Grammar

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DCG lang." $ do
    it "should not validate cyclic grammars" $ do
        let grammar = Grammar "S" [(Lhs "S") ~> (Lexem "dupa")]
        (validate grammar) `shouldBe` False