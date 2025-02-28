module PropLogicTest (propLogicTests) where

import ParseTools
import PropLogicFuncs
import PropLogicParser
import PropLogicTypes (Form)
import TestTypes

propNamesExs :: TestBlock
propNamesExs =
  TestBlock
    ( [ ("-&[p,v[p,-&[q,r2]]]", Just ["p", "q", "r2"]),
        ("&[r,q]", Just ["q", "r"])
      ],
      propNames . getParsed
    )

depthExs :: TestBlock
depthExs =
  TestBlock
    ( [ ("-&[p,v[-p,-&[q,r2]]]", Just 5),
        ("v[p,-q]", Just 2),
        ("&[r,q]", Just 1)
      ],
      depth . getParsed
    )

opsNrExs :: TestBlock
opsNrExs =
  TestBlock
    ( [ ("-&[p,v[-p,-&[q,r2]]]", Just 6),
        ("v[p,&[-p,v[-p,-q]]]", Just 6),
        ("&[r,q]", Just 1),
        ("&[r,q,z]", Nothing),
        ("&[r,q", Nothing)
      ],
      opsNr . getParsed
    )

evalExs :: TestBlock
evalExs =
  TestBlock
    ( [ (([("p", True), ("q", False)], "v[p,q]"), Just True),
        (([("p", True), ("q", False), ("q2", True)], "&[q2,-v[-p,q]]"), Just True),
        (([("p", True), ("q2", True)], "&[q2,-v[-p,q]]"), Nothing),
        (([("p", True)], "&[p,-p]"), Just False),
        (([], "&[p,-p]"), Nothing)
      ],
      handleEval
    )

handleEval :: ([(String, Bool)], String) -> Bool
handleEval (tbl, frm) = eval tbl (getParsed frm)

propLogicTests :: [TestBlock]
propLogicTests = [propNamesExs, depthExs, opsNrExs, evalExs]

getParsed :: String -> Form
getParsed = unsafeRight . parse parseForm

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)
