module Main where

import PredLogicFuncs
import PredLogicParser
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

toNnf :: String -> String
toNnf = show . nnf . parseFormulaUnsafe

-- Test cases from https://en.wikipedia.org/wiki/Negation_normal_form
equivs :: [(String, String)]
equivs =
  [ ("(R[x]==>R[y])", "disj[~R[x],R[y]]"),
    ("(R[x]<=>R[y])", "conj[disj[~R[x],R[y]],disj[R[x],~R[y]]]"),
    ("~disj[R[x],R[y]]", "conj[~R[x],~R[y]]"),
    ("~conj[R[x],R[y]]", "disj[~R[x],~R[y]]"),
    ("~~R[x]", "R[x]"),
    ("~Ex R[x]", "Ax ~R[x]"),
    ("~Ax R[x]", "Ex ~R[x]")
  ]

tests :: Test
tests = TestList $ doTest <$> equivs

doTest :: (String, String) -> Test
doTest (given, expected) = TestLabel "ok" (TestCase (assertEqual "a" (toNnf given) expected))

-- >>> parseFormulaUnsafe "(R[x]<=>R[y])"
-- (R[x]<=>R[y])
