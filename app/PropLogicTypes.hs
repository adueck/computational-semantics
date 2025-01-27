module PropLogicTypes
  ( Form (..),
  )
where

data Form = P String | Ng Form | Cnj [Form] | Dsj [Form] deriving (Eq)

instance Show Form where
  show (P name) = name
  show (Ng f) = '-' : show f
  show (Cnj fs) = '&' : show fs
  show (Dsj fs) = 'v' : show fs