module PredLogicTypes
  ( Variable (..),
    Formula (..),
    Term (..),
  )
where

type Name = String

type Index = [Int]

data Variable = Variable Name Index deriving (Eq, Ord)

instance Show Variable where
  show (Variable name []) = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is) = name ++ showInts is
    where
      showInts [] = ""
      showInts [i] = show i
      showInts (i : is1) = show i ++ "_" ++ showInts is1

data Formula a
  = Atom String [a]
  | Eq a a
  | Neg (Formula a)
  | Impl (Formula a) (Formula a)
  | Equi (Formula a) (Formula a)
  | Conj [Formula a]
  | Disj [Formula a]
  | Forall Variable (Formula a)
  | Exists Variable (Formula a)
  deriving (Eq)

instance (Show a) => Show (Formula a) where
  show (Atom s []) = s
  show (Atom s xs) = s ++ show xs
  show (Eq t1 t2) = show t1 ++ "==" ++ show t2
  show (Neg form) = '~' : show form
  show (Impl f1 f2) = "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
  show (Equi f1 f2) = "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"
  show (Conj []) = "true"
  show (Conj fs) = "conj" ++ show fs
  show (Disj []) = "false"
  show (Disj fs) = "disj" ++ show fs
  show (Forall v f) = "A" ++ show v ++ (' ' : show f)
  show (Exists v f) = "E" ++ show v ++ (' ' : show f)

data Term = Var Variable | Struct String [Term]
  deriving (Eq, Ord)

instance Show Term where
  show (Var v) = show v
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

-- tx, ty, tf :: Term
-- tx = Var (Variable "x" [])
-- ty = Var (Variable "y" [])
-- tf = Struct "f" [Var (Variable "x" []), Var (Variable "y" [])]
