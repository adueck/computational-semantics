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
      \(tbl, frm) -> eval tbl (getParsed frm)
    )

evalAltExs :: TestBlock
evalAltExs =
  TestBlock
    ( [ ((["p"], "v[p,q]"), Just True),
        ((["p", "q2"], "&[q2,-v[-p,q]]"), Just True),
        ((["p"], "&[p,-p]"), Just False)
      ],
      \(tbl, frm) -> evalAlt tbl (getParsed frm)
    )

tautologyExs :: TestBlock
tautologyExs =
  TestBlock
    ( [ ("&[p,-p]", Just False),
        ("-&[p,-p]", Just True),
        ("v[p,-p]", Just True),
        ("p", Just False)
      ],
      tautology . getParsed
    )

satisfiableExs :: TestBlock
satisfiableExs =
  TestBlock
    ( [ ("&[p,-p]", Just False),
        ("v[p1,p2,p3,p4]", Just True),
        ("p", Just True)
      ],
      satisfiable . getParsed
    )

contradictionExs :: TestBlock
contradictionExs =
  TestBlock
    ( [ ("&[p,-p]", Just True),
        ("-&[p,-p]", Just False),
        ("p", Just False)
      ],
      contradiction . getParsed
    )

impliesExs :: TestBlock
impliesExs =
  TestBlock
    ( [ (("--p", "p"), Just True),
        (("-&[p,q]", "v[-p,-q]"), Just True),
        (("-&[p,q]", "&[-p,-q]"), Just False),
        (("-&[p,-v[p,-q]]", "v[-p,-&[-p,q]]"), Just True),
        -- and reversed
        (("p", "--p"), Just True),
        (("v[-p,-q]", "-&[p,q]"), Just True),
        (("v[-p,-&[-p,q]]", "-&[p,-v[p,-q]]"), Just True)
      ],
      \(s1, s2) -> implies (getParsed s1) (getParsed s2)
    )

impliesLExs :: TestBlock
impliesLExs =
  TestBlock
    ( [ ((["--p", "----p"], "p"), Just True),
        ((["-&[p,q]", "v[---p,---q]"], "v[-p,-q]"), Just True),
        ((["-&[p,q]"], "&[-p,-q]"), Just False),
        ((["-&[p,-v[p,-q]]"], "v[-p,-&[-p,q]]"), Just True),
        ((["-&[p,q]", "--v[-p,-q]"], "v[-p,-q]"), Just True),
        ((["-&[p,q]", "--v[-p,-q]"], "v[-p,q]"), Just False),
        ((["&[p,v[p,q]]", "v[&[p,p],&[p,q]]", "v[p,&[p,q]]"], "p"), Just True)
      ],
      \(sl, s2) -> impliesL (map getParsed sl) (getParsed s2)
    )

propEquivExs :: TestBlock
propEquivExs =
  TestBlock
    ( [ -- Idempotent or tautology laws
        (("&[p,p]", "p"), Just True),
        (("v[p,p]", "p"), Just True),
        -- Double negation law
        (("--p", "p"), Just True),
        (("---p", "p"), Just False),
        -- Commutative laws
        (("&[p,q]", "&[p,q]"), Just True),
        (("&[p,q]", "v[p,q]"), Just False),
        (("v[p,q]", "v[p,q]"), Just True),
        -- Associative laws
        (("v[v[p,q],r]", "v[p,v[q,r]]"), Just True),
        (("&[&[p,q],r]", "&[p,&[q,r]]"), Just True),
        -- Distributive laws
        (("v[p,&[q,r]]", "&[v[p,q],v[p,r]]"), Just True),
        (("&[p,v[q,r]]", "v[&[p,q],&[p,r]]"), Just True),
        -- De Morgan's laws
        (("-&[p,q]", "v[-p,-q]"), Just True),
        (("-v[p,q]", "&[-p,-q]"), Just True),
        -- Absorption laws
        (("v[p,&[p,q]]", "p"), Just True),
        (("&[p,v[p,q]]", "p"), Just True)
      ],
      \(s1, s2) -> propEquiv (getParsed s1) (getParsed s2)
    )

propLogicTests :: [TestBlock]
propLogicTests =
  [ propNamesExs,
    depthExs,
    opsNrExs,
    evalExs,
    tautologyExs,
    satisfiableExs,
    contradictionExs,
    impliesExs,
    impliesLExs,
    propEquivExs,
    evalAltExs
  ]

getParsed :: String -> Form
getParsed = unsafeRight . parse parseForm

unsafeRight :: (Show a) => Either a b -> b
unsafeRight (Right b) = b
unsafeRight (Left a) = error (show a)
