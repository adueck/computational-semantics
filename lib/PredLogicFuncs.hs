module PredLogicFuncs where

import Data.List
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

withoutIDs :: Formula a -> Formula a
withoutIDs (Atom x v) = Atom x v
withoutIDs (Eq l r) = Eq l r
withoutIDs (Neg f) = Neg (withoutIDs f)
withoutIDs (Impl l r) = withoutIDs (Disj [Neg l, r])
withoutIDs (Equi l r) = withoutIDs (Conj [Disj [Neg l, r], Disj [l, Neg r]])
withoutIDs (Conj fs) = Conj (fmap withoutIDs fs)
withoutIDs (Disj fs) = Disj (fmap withoutIDs fs)
withoutIDs (Forall v f) = Forall v (withoutIDs f)
withoutIDs (Exists v f) = Forall v (withoutIDs f)

nnf :: Formula a -> Formula a
nnf (Atom x v) = Atom x v
nnf (Eq l r) = Eq l r
nnf (Conj fs) = Conj (fmap nnf fs)
nnf (Disj fs) = Disj (fmap nnf fs)
nnf (Neg (Conj fs)) = Disj (fmap (nnf . Neg) fs)
nnf (Neg (Disj fs)) = Conj (fmap (nnf . Neg) fs)
nnf (Neg (Neg x)) = nnf x
nnf (Neg (Forall v f)) = nnf (Exists v (Neg (nnf f)))
nnf (Neg (Exists v f)) = nnf (Forall v (Neg (nnf f)))
nnf (Neg x) = Neg (nnf x)
nnf (Impl l r) = nnf (withoutIDs (Impl l r))
nnf (Equi l r) = nnf (withoutIDs (Equi l r))
nnf (Forall v f) = Forall v (nnf f)
nnf (Exists v f) = Exists v (nnf f)

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

varsInTerm :: Term -> [Variable]
varsInTerm (Var v) = [v]
varsInTerm (Struct _ ts) = varsInTerms ts

varsInTerms :: [Term] -> [Variable]
varsInTerms = nub . concatMap varsInTerm

varsInForm :: Formula Term -> [Variable]
varsInForm (Atom _ as) = varsInTerms as
varsInForm (Eq a b) = nub (varsInTerm a ++ varsInTerm b)
varsInForm (Neg a) = varsInForm a
varsInForm (Impl a b) = nub (varsInForm a ++ varsInForm b)
varsInForm (Equi a b) = nub (varsInForm a ++ varsInForm b)
varsInForm (Conj as) = varsInForms as
varsInForm (Disj as) = varsInForms as
varsInForm (Forall v a) = nub (v : varsInForm a)
varsInForm (Exists v a) = nub (v : varsInForm a)

varsInForms :: [Formula Term] -> [Variable]
varsInForms = nub . concatMap varsInForm

freeVarsInForm :: Formula Term -> [Variable]
freeVarsInForm (Atom _ as) = varsInTerms as
freeVarsInForm (Eq a b) = nub (varsInTerm a ++ varsInTerm b)
freeVarsInForm (Neg a) = freeVarsInForm a
freeVarsInForm (Impl a b) = nub (freeVarsInForm a ++ freeVarsInForm b)
freeVarsInForm (Equi a b) = nub (freeVarsInForm a ++ freeVarsInForm b)
freeVarsInForm (Conj as) = freeVarsInForms as
freeVarsInForm (Disj as) = freeVarsInForms as
freeVarsInForm (Forall (Variable n _) a) = filter (\(Variable m _) -> n /= m) (freeVarsInForm a)
freeVarsInForm (Exists (Variable n _) a) = filter (\(Variable m _) -> n /= m) (freeVarsInForm a)

openForm :: Formula Term -> Bool
openForm = null . freeVarsInForm

freeVarsInForms :: [Formula Term] -> [Variable]
freeVarsInForms = nub . concatMap freeVarsInForm

-- >>> nnf (parseFormulaUnsafe "~Ex R[x]")
-- Ex ~R[x]
