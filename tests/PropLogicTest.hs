module PropLogicTest (propLogicTests) where

import PropLogicFuncs
import PropLogicParser
import PropLogicTypes (Form)
import TestTypes

propNamesExs :: TestBlock
propNamesExs =
  TestBlock
    ( [ ("-&[p,v[p,-&[q,r2]]]", ["p", "q", "r2"]),
        ("&[r,q]", ["q", "r"])
      ],
      propNames . getParsed
    )

depthExs :: TestBlock
depthExs =
  TestBlock
    ( [ ("-&[p,v[-p,-&[q,r2]]]", 5),
        ("v[p,-q]", 2),
        ("&[r,q]", 1)
      ],
      depth . getParsed
    )

opsNrExs :: TestBlock
opsNrExs =
  TestBlock
    ( [ ("-&[p,v[-p,-&[q,r2]]]", 6),
        ("v[p,&[-p,v[-p,-q]]]", 6),
        ("&[r,q]", 1)
      ],
      opsNr . getParsed
    )

propLogicTests :: [TestBlock]
propLogicTests = [propNamesExs, depthExs, opsNrExs]

getParsed :: String -> Form
getParsed = unsafeRight . parse parseForm

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)
