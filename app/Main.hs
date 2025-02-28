module Main where

import PredLogicParser
import PredLogicTypes (Formula (..), Variable (Variable))
import Test.HUnit

newtype TestBlock a b = TestBlock ([(a, b)], a -> b)

makeTestBlock :: (Show a, Eq b) => [(a, b)] -> (a -> b) -> TestBlock a b
makeTestBlock pairs func = TestBlock (pairs, func)

main :: IO ()
main = runTestTTAndExit tests

parseExs :: TestBlock String (Formula Variable)
parseExs =
  makeTestBlock
    [ ("R[x]", Atom "R" [Variable "x" []]),
      ("R[x,z3,y]", Atom "R" [Variable "x" [], Variable "z3" [], Variable "y" []]),
      ("x==z", Eq (Variable "x" []) (Variable "y" [])),
      ("~R[x]", Neg (Atom "R" [Variable "x" []])),
      ("(R[x]==>R[y])", Impl (Atom "R" [Variable "x" []]) (Atom "R" [Variable "y" []])),
      ("(R[x]<=>R[y])", Equi (Atom "R" [Variable "x" []]) (Atom "R" [Variable "y" []])),
      ("conj[R[x],R[y]]", Conj [Atom "R" [Variable "x" []], Atom "R" [Variable "y" []]]),
      ("Ax R[x]", Forall (Variable "x" []) $ Atom "R" [Variable "x" []]),
      ("Ey R[x]", Exists (Variable "y" []) $ Atom "R" [Variable "x" []]),
      ("(Ex R[x,y]==>R[y])", Impl (Exists (Variable "x" []) $ Atom "R" [Variable "x" [], Variable "y" []]) (Atom "R" [Variable "y" []]))
    ]
    (unsafeRight . parse (parseFormula parseVar))

-- toNnf :: String -> String
-- toNnf = show . nnf . parseVarFormulaUnsafe

-- -- Test cases from https://en.wikipedia.org/wiki/Negation_normal_form
-- equivs :: [(String, String)]
-- equivs =
--   [ ("(R[x]==>R[y])", "disj[~R[x],R[y]]"),
--     ("(R[x]<=>R[y])", "conj[disj[~R[x],R[y]],disj[R[x],~R[y]]]"),
--     ("~disj[R[x],R[y]]", "conj[~R[x],~R[y]]"),
--     ("~conj[R[x],R[y]]", "disj[~R[x],~R[y]]"),
--     ("~~R[x]", "R[x]"),
--     ("~Ex R[x]", "Ax ~R[x]"),
--     ("~Ax R[x]", "Ex ~R[x]")
--   ]

makeTestLists :: (Show a, Eq b, Show b) => TestBlock a b -> [Test]
makeTestLists (TestBlock (equivs, f)) = map (doTest f) equivs

tests :: Test
tests = TestList $ concatMap makeTestLists [parseExs]

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)

doTest :: (Eq b, Show a, Show b) => (a -> b) -> (a, b) -> Test
doTest f (given, expected) = TestLabel (show given) (TestCase (assertEqual (show given) (f given) expected))

-- >>> parseFormulaUnsafe "(R[x]<=>R[y])"
-- (R[x]<=>R[y])
