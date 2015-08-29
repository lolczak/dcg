module Main where

import System.Environment
import Data.Text
import DCG.GrammarParser
import DCG.ChartParser
import Control.Monad

main :: IO()
main =
    do args <- getArgs
       let grammarPath = args !! 0
       let sentence = args !! 1
       grammarString <- readFile grammarPath
       case parseGrammar grammarString of
            Left err -> putStrLn $ show err
            Right (lexicon, grammar) -> do let u = split (== ' ') $ pack sentence
                                           let utterance = fmap unpack u
                                           let result = parseGCD lexicon grammar utterance
                                           _ <- forM result (\item -> putStrLn $ show item)
                                           return ()
       return ()