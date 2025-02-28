module PredLogicTest (predLogicTests) where

import PredLogicFuncs
import PredLogicParser
import PredLogicTypes
import TestTypes

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
        ("~Ax R[x]", "Ex ~R[x]"),
        -- Plus recursion
        ("~disj[R[x],~conj[R[x],R[y]]]", "conj[~R[x],conj[R[x],R[y]]]"),
        ("~Ax ~Ex R[x]", "Ex Ex R[x]")
      ],
      show . nnf . unsafeRight . parse (parseFormula parseVar)
    )

varsInFormExs :: TestBlock
varsInFormExs =
  TestBlock
    ( [("~disj[R[x],R[f[x1,x2]]]", [Variable "x" [], Variable "x1" [], Variable "x2" []])],
      varsInForm . unsafeRight . parse (parseFormula parseTerm)
    )

freeVarsInFormExs :: TestBlock
freeVarsInFormExs =
  TestBlock
    ( [ ("~disj[R[x],R[f[x1,x2]]]", [Variable "x" [], Variable "x1" [], Variable "x2" []]),
        ("Ax Ex1 ~disj[R[x],R[f[x1,x2]]]", [Variable "x2" []]),
        ("Ax Ex1 ~conj[R[x],Ex2 R[f[x1,x2]]]", [])
      ],
      freeVarsInForm . unsafeRight . parse (parseFormula parseTerm)
    )

openFormExs :: TestBlock
openFormExs =
  TestBlock
    ( [ ("~disj[R[x],R[f[x1,x2]]]", False),
        ("Ax Ex1 ~disj[R[x],R[f[x1,x2]]]", False),
        ("Ax Ex1 ~conj[R[x],Ex2 R[f[x1,x2]]]", True)
      ],
      openForm . unsafeRight . parse (parseFormula parseTerm)
    )

predLogicTests :: [TestBlock]
predLogicTests = [parseExs, collectUnboundExs, nnfExs, varsInFormExs]

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)
