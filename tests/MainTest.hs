module Main where

import Control.Exception (SomeException, evaluate, try)
import PredLogicFuncs
import PredLogicParser
import PredLogicTest (predLogicTests)
import PredLogicTypes
import PropLogicTest (propLogicTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import TestTypes

allMods :: [[TestBlock]]
allMods = [propLogicTests, predLogicTests]

main :: IO ()
main = do
  counts <- mapM (runTestTT . getTest) (concat allMods)
  let failed = any (\c -> failures c > 0 || errors c > 0) counts
  if failed then exitFailure else exitSuccess

getTest :: TestBlock -> Test
getTest (TestBlock (equivs, f)) = TestList $ map (doTest f) equivs

doTest :: (Eq b, Show a, Show b) => (a -> b) -> (a, Maybe b) -> Test
doTest f (given, Just expected) = TestLabel (show given) (TestCase (assertEqual (show given) (f given) expected))
doTest f (given, Nothing) = TestLabel (show given) (TestCase (assertThrows f given))

assertThrows :: (Show a) => (a -> b) -> a -> Assertion
assertThrows f input = do
  result <- try (evaluate (f input))
  case result of
    Left (_ :: SomeException) -> return ()
    Right _ -> assertFailure $ "Expected an exception, but got successful result for input: " ++ show input
