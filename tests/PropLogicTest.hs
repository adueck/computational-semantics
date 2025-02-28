module PropLogicTest (propLogicTests) where

import PropLogicFuncs
import PropLogicParser
import TestTypes

propNamesExs :: TestBlock
propNamesExs =
  TestBlock
    ( [ ("-&[p,v[p,-&[q,r2]]]", ["p", "q", "r2"]),
        ("&[r,q,v]", ["r", "q", "v"])
      ],
      propNames . unsafeRight . parse parseForm
    )

depthExs :: TestBlock
depthExs =
  TestBlock
    ( [ ("-&[p,v[-p,-&[q,r2]]]", 5),
        ("v[p,-q, -p]", 2),
        ("&[r,q,v]", 1)
      ],
      depth . unsafeRight . parse parseForm
    )

opsNrExs :: TestBlock
opsNrExs =
  TestBlock
    ( [ ("-&[p,v[p,-&[q,r2]]]", 5),
        ("v[p,-q, &[-p,v[-p,-q]]]", 7),
        ("&[r,q,v]", 1)
      ],
      opsNr . unsafeRight . parse parseForm
    )

propLogicTests :: [TestBlock]
propLogicTests = [propNamesExs, depthExs, opsNrExs]

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)
