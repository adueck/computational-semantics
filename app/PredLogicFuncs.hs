module PredLogicFuncs where

import PredLogicTypes

closedForm :: Formula Variable -> Bool
closedForm f = null varList
  where
    varList = collectUnbound f []

collectUnbound :: Formula Variable -> [String] -> [String]
collectUnbound (Atom _ vars) bound = map (\(Variable n _) -> n) (filter (\(Variable n _) -> n `notElem` bound) vars)
collectUnbound (Eq l r) bound = map (\(Variable n _) -> n) (filter (\(Variable n _) -> n `notElem` bound) [l, r])
collectUnbound (Neg f) bound = collectUnbound f bound
collectUnbound (Impl l r) bound = collectUnbound l bound ++ collectUnbound r bound
collectUnbound (Equi l r) bound = collectUnbound l bound ++ collectUnbound r bound
collectUnbound (Conj fs) bound = foldr collectUnbound bound fs
collectUnbound (Disj fs) bound = foldr collectUnbound bound fs
collectUnbound (Forall (Variable s _) f) bound = collectUnbound f (s : bound)
collectUnbound (Exists (Variable s _) f) bound = collectUnbound f (s : bound)

withoutIDs :: Formula Variable -> Formula Variable
withoutIDs (Atom x v) = Atom x v
withoutIDs (Eq l r) = Eq l r
withoutIDs (Neg f) = Neg (withoutIDs f)
withoutIDs (Impl l r) = withoutIDs (Disj [Neg l, r])
withoutIDs (Equi l r) = withoutIDs (Conj [Disj [Neg l, r], Disj [l, Neg r]])
withoutIDs (Conj fs) = Conj (fmap withoutIDs fs)
withoutIDs (Disj fs) = Disj (fmap withoutIDs fs)
withoutIDs (Forall v f) = Forall v (withoutIDs f)
withoutIDs (Exists v f) = Forall v (withoutIDs f)

nnf :: Formula Variable -> Formula Variable
nnf (Atom x v) = Atom x v
nnf (Eq l r) = Eq l r
nnf (Conj fs) = Conj (fmap nnf fs)
nnf (Disj fs) = Disj (fmap nnf fs)
nnf (Neg (Conj fs)) = Disj (fmap Neg fs)
nnf (Neg (Disj fs)) = Conj (fmap Neg fs)
nnf (Neg (Neg x)) = nnf x
nnf (Neg (Forall v f)) = Exists v (nnf (Neg f))
nnf (Neg (Exists v f)) = Forall v (nnf (Neg f))
nnf (Neg x) = Neg x
nnf (Impl l r) = withoutIDs (Impl l r)
nnf (Equi l r) = withoutIDs (Equi l r)
nnf (Forall v f) = Forall v (nnf f)
nnf (Exists v f) = Exists v (nnf f)

-- >>> nnf (parseFormulaUnsafe "~Ex R[x]")
-- Ex ~R[x]
