module PrepLogicFuncs where

import Data.List (nub, sort)
import PropLogicParser
import PropLogicTypes

countOpsInList :: [Form] -> Int
countOpsInList = foldr (\x a -> a + opsNr x) 0

opsNr :: Form -> Int
opsNr (P _) = 0
opsNr (Ng f) = 1 + opsNr f
opsNr (Cnj fs) = countOpsInList fs
opsNr (Dsj fs) = countOpsInList fs

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

-- >>> (propNames . parseFormUnsafe) "-&[p,v[p,-&[q,r2]]]"
-- ["p","q","r2"]
