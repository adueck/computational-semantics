module PredLogicTest (predLogicTests) where

import PredLogicFuncs
import PredLogicParser
import PredLogicTypes
import TestTypes
import Text.Parsec (Parsec, getInput)

parseExs :: TestBlock
parseExs =
  TestBlock
    ( [ ("R[x]", Just $ Atom "R" [Variable "x" []]),
        ("R[x", Nothing),
        ("R[x,z3,y]", Just $ Atom "R" [Variable "x" [], Variable "z3" [], Variable "y" []]),
        ("x==z", Just $ Eq (Variable "x" []) (Variable "z" [])),
        ("~R[x]", Just $ Neg (Atom "R" [Variable "x" []])),
        ("(R[x]==>R[y])", Just $ Impl (Atom "R" [Variable "x" []]) (Atom "R" [Variable "y" []])),
        ("R[x]==>R[y]", Nothing),
        ("(R[x]<=>R[y])", Just $ Equi (Atom "R" [Variable "x" []]) (Atom "R" [Variable "y" []])),
        ("conj[R[x],R[y]]", Just $ Conj [Atom "R" [Variable "x" []], Atom "R" [Variable "y" []]]),
        ("Ax R[x]", Just $ Forall (Variable "x" []) $ Atom "R" [Variable "x" []]),
        ("Ey R[x]", Just $ Exists (Variable "y" []) $ Atom "R" [Variable "x" []]),
        ("(Ex R[x,y]==>R[y])", Just $ Impl (Exists (Variable "x" []) $ Atom "R" [Variable "x" [], Variable "y" []]) (Atom "R" [Variable "y" []])),
        ("(Ex R[x,y]=>R[y])", Nothing)
      ],
      getParsed parseVar
    )

-- >>> getParsed parseVar $ "R[x]==>R[y]"
-- R[x]

collectUnboundExs :: TestBlock
collectUnboundExs =
  TestBlock
    ( [ ("Ax R[x,y]", Just ["y"]),
        ("x==z2", Just ["x", "z2"]),
        ("Ex Ay R[x,y,z]", Just ["z"])
      ],
      \s -> (collectUnbound $ getParsed parseVar s) []
    )

nnfExs :: TestBlock
nnfExs =
  TestBlock
    ( -- Test cases from https://en.wikipedia.org/wiki/Negation_normal_form
      [ ("(R[x]==>R[y])", Just "disj[~R[x],R[y]]"),
        ("(R[x]<=>R[y])", Just "conj[disj[~R[x],R[y]],disj[R[x],~R[y]]]"),
        ("~disj[R[x],R[y]]", Just "conj[~R[x],~R[y]]"),
        ("~conj[R[x],R[y]]", Just "disj[~R[x],~R[y]]"),
        ("~~R[x]", Just "R[x]"),
        ("~Ex R[x]", Just "Ax ~R[x]"),
        ("~Ax R[x]", Just "Ex ~R[x]"),
        -- Plus recursion
        ("~disj[R[x],~conj[R[x],R[y]]]", Just "conj[~R[x],conj[R[x],R[y]]]"),
        ("~Ax ~Ex R[x]", Just "Ex Ex R[x]")
      ],
      show . nnf . getParsed parseVar
    )

varsInFormExs :: TestBlock
varsInFormExs =
  TestBlock
    ( [("~disj[R[x],R[f[x1,x2]]]", Just [Variable "x" [], Variable "x1" [], Variable "x2" []])],
      varsInForm . getParsed parseTerm
    )

freeVarsInFormExs :: TestBlock
freeVarsInFormExs =
  TestBlock
    ( [ ("~disj[R[x],R[f[x1,x2]]]", Just [Variable "x" [], Variable "x1" [], Variable "x2" []]),
        ("Ax Ex1 ~disj[R[x],R[f[x1,x2]]]", Just [Variable "x2" []]),
        ("Ax Ex1 ~conj[R[x],Ex2 R[f[x1,x2]]]", Just [])
      ],
      freeVarsInForm . getParsed parseTerm
    )

openFormExs :: TestBlock
openFormExs =
  TestBlock
    ( [ ("~disj[R[x],R[f[x1,x2]]]", Just False),
        ("Ax Ex1 ~disj[R[x],R[f[x1,x2]]]", Just False),
        ("Ax Ex1 ~conj[R[x],Ex2 R[f[x1,x2]]]", Just True)
      ],
      openForm . getParsed parseTerm
    )

predLogicTests :: [TestBlock]
predLogicTests = [parseExs, collectUnboundExs, nnfExs, freeVarsInFormExs, varsInFormExs, openFormExs]

getParsed :: Parsec String () a -> String -> Formula a
getParsed f = unsafeRight . parse (parseFormula f)

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)

parseWithRest :: Parsec String () a -> Parsec String () (a, String)
parseWithRest pp = do
  parsed <- pp -- Parse a word
  rest <- getInput -- Get the remaining unparsed input
  return (parsed, rest)

--- >>> (parse (parseFormula parseTerm)) "R[x]==>R[y]"
-- Right R[x]
