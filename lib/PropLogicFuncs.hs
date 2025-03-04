module PropLogicFuncs where

import Data.List (nub, sort)
import PropLogicTypes

countOpsInList :: [Form] -> Int
countOpsInList = foldr (\x a -> a + opsNr x) 0

opsNr :: Form -> Int
opsNr (P _) = 0
opsNr (Ng f) = 1 + opsNr f
opsNr (Cnj fs) = 1 + countOpsInList fs
opsNr (Dsj fs) = 1 + countOpsInList fs

findDepthInList :: [Form] -> Int
findDepthInList = maximum . fmap depth

depth :: Form -> Int
depth (P _) = 0
depth (Ng f) = 1 + depth f
depth (Cnj fs) = 1 + findDepthInList fs
depth (Dsj fs) = 1 + findDepthInList fs

propNamesList :: [Form] -> [String]
propNamesList = sort . nub . concatMap propNames

propNames :: Form -> [String]
propNames (P p) = [p]
propNames (Ng f) = propNames f
propNames (Cnj fs) = propNamesList fs
propNames (Dsj fs) = propNamesList fs

genVals :: [String] -> [[(String, Bool)]]
genVals [] = [[]]
genVals (n : ns) =
  map ((n, True) :) (genVals ns)
    ++ map ((n, False) :) (genVals ns)

allVals :: Form -> [[(String, Bool)]]
allVals = genVals . propNames

eval :: [(String, Bool)] -> Form -> Bool
eval tbl (P c) = case v of
  Just val -> val
  Nothing -> error ("ERROR: " ++ show c ++ " not defined")
  where
    v = lookup c tbl
eval tbl (Ng f) = not (eval tbl f)
eval tbl (Cnj fs) = all (eval tbl) fs
eval tbl (Dsj fs) = any (eval tbl) fs

evalAlt :: [String] -> Form -> Bool
evalAlt tbl (P c) = c `elem` tbl
evalAlt tbl (Ng f) = not (evalAlt tbl f)
evalAlt tbl (Cnj fs) = all (evalAlt tbl) fs
evalAlt tbl (Dsj fs) = any (evalAlt tbl) fs

tautology :: Form -> Bool
tautology f = all (`eval` f) (allVals f)

satisfiable :: Form -> Bool
satisfiable f = any (`eval` f) (allVals f)

contradiction :: Form -> Bool
contradiction = not . satisfiable

implies :: Form -> Form -> Bool
implies f1 f2 = contradiction (Cnj [f1, Ng f2])

impliesL :: [Form] -> Form -> Bool
impliesL = implies . Cnj

propEquiv :: Form -> Form -> Bool
propEquiv f1 f2 = f1 `implies` f2 && f2 `implies` f1

update :: [[(String, Bool)]] -> Form -> [[(String, Bool)]]
update vals f = [v | v <- vals, eval v f]
