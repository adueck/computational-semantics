{-# LANGUAGE GADTs #-}

module Main where

import PredLogicFuncs
import PredLogicParser
import PredLogicTypes
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

data TestBlock where
  TestBlock :: (Show a, Eq b, Show b) => ([(a, b)], a -> b) -> TestBlock

main :: IO ()
main = do
  counts <- mapM (runTestTT . getTest) allTestBlocks
  let failed = any ((<) 0 . failures) counts
  if failed then exitFailure else exitSuccess

parseExs :: TestBlock
parseExs =
  TestBlock
    ( [ ("R[x]", Atom "R" [Variable "x" []]),
        ("R[x,z3,y]", Atom "R" [Variable "x" [], Variable "z3" [], Variable "y" []]),
        ("x==z", Eq (Variable "x" []) (Variable "z" [])),
        ("~R[x]", Neg (Atom "R" [Variable "x" []])),
        ("(R[x]==>R[y])", Impl (Atom "R" [Variable "x" []]) (Atom "R" [Variable "y" []])),
        ("(R[x]<=>R[y])", Equi (Atom "R" [Variable "x" []]) (Atom "R" [Variable "y" []])),
        ("conj[R[x],R[y]]", Conj [Atom "R" [Variable "x" []], Atom "R" [Variable "y" []]]),
        ("Ax R[x]", Forall (Variable "x" []) $ Atom "R" [Variable "x" []]),
        ("Ey R[x]", Exists (Variable "y" []) $ Atom "R" [Variable "x" []]),
        ("(Ex R[x,y]==>R[y])", Impl (Exists (Variable "x" []) $ Atom "R" [Variable "x" [], Variable "y" []]) (Atom "R" [Variable "y" []]))
      ],
      unsafeRight . parse (parseFormula parseVar)
    )

collectUnboundExs :: TestBlock
collectUnboundExs =
  TestBlock
    ( [ ("Ax R[x,y]", ["y"]),
        ("x==z2", ["x", "z2"]),
        ("Ex Ay R[x,y,z]", ["z"])
      ],
      \s -> collectUnbound ((unsafeRight . parse (parseFormula parseVar)) s) []
    )

nnfExs :: TestBlock
nnfExs =
  TestBlock
    ( -- Test cases from https://en.wikipedia.org/wiki/Negation_normal_form
      [ ("(R[x]==>R[y])", "disj[~R[x],R[y]]"),
        ("(R[x]<=>R[y])", "conj[disj[~R[x],R[y]],disj[R[x],~R[y]]]"),
        ("~disj[R[x],R[y]]", "conj[~R[x],~R[y]]"),
        ("~conj[R[x],R[y]]", "disj[~R[x],~R[y]]"),
        ("~~R[x]", "R[x]"),
        ("~Ex R[x]", "Ax ~R[x]"),
        ("~Ax R[x]", "Ex ~R[x]")
      ],
      show . nnf . unsafeRight . parse (parseFormula parseVar)
    )

allTestBlocks :: [TestBlock]
allTestBlocks = [parseExs, collectUnboundExs, nnfExs]

getTest :: TestBlock -> Test
getTest (TestBlock (equivs, f)) = TestList $ map (doTest f) equivs

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)

doTest :: (Eq b, Show a, Show b) => (a -> b) -> (a, b) -> Test
doTest f (given, expected) = TestLabel (show given) (TestCase (assertEqual (show given) (f given) expected))
