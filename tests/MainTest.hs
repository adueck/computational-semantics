module Main where

import PredLogicFuncs
import PredLogicParser
import PredLogicTest (predLogicTests)
import PredLogicTypes
import PropLogicTest (propLogicTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import TestTypes

allMods = [predLogicTests, propLogicTests]

main :: IO ()
main = do
  counts <- mapM (runTestTT . getTest) (concat allMods)
  let failed = any ((<) 0 . failures) counts
  if failed then exitFailure else exitSuccess

getTest :: TestBlock -> Test
getTest (TestBlock (equivs, f)) = TestList $ map (doTest f) equivs

doTest :: (Eq b, Show a, Show b) => (a -> b) -> (a, b) -> Test
doTest f (given, expected) = TestLabel (show given) (TestCase (assertEqual (show given) expected (f given)))
