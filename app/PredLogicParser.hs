module PredLogicParser
  ( parse,
    parseFormula,
    parseVar,
  )
where

import Control.Applicative
import Data.Functor.Identity
import PredLogicTypes
import Text.Parsec qualified as Parsec

parse :: (Parsec.Stream s Data.Functor.Identity.Identity t) => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

parseFormula :: Parsec.Parsec String () a -> Parsec.Parsec String () (Formula a)
parseFormula pp =
  foldl1
    (<|>)
    (($ pp) <$> [parseAtom, parseEq, parseNeg, parseEquiOrImp, parseConjOrDisj, parseForallOrExists])

parseList :: Parsec.Parsec String () a -> Parsec.Parsec String () [a]
parseList pp =
  Parsec.between (Parsec.char '[') (Parsec.char ']') $
    Parsec.sepBy1 pp (Parsec.spaces >> Parsec.char ',' >> Parsec.spaces)

parseVar :: Parsec.Parsec String () Variable
parseVar = do
  letter <- Parsec.oneOf "xyz"
  digits <- Parsec.many Parsec.digit
  return (Variable (letter : digits) [])

parseTerm :: Parsec.Parsec String () Term
parseTerm = (Var <$> parseVar) <|> parseStruct

parseStruct :: Parsec.Parsec String () Term
parseStruct = do
  _ <- Parsec.char 'f'
  digits <- Parsec.many Parsec.digit
  terms <- parseList parseTerm
  return (Struct ('f' : digits) terms)

parseAtom :: Parsec.Parsec String () a -> Parsec.Parsec String () (Formula a)
parseAtom pp = do
  _ <- Parsec.char 'R'
  vars <- parseList pp
  return $ Atom "R" vars

parseEq :: Parsec.Parsec String () a -> Parsec.Parsec String () (Formula a)
parseEq pp = do
  l <- pp
  _ <- Parsec.string "=="
  Eq l <$> pp

parseNeg :: Parsec.Parsec String () a -> Parsec.Parsec String () (Formula a)
parseNeg pp = do
  _ <- Parsec.char '~'
  f <- parseFormula pp
  return $ Neg f

parseEquiOrImp :: Parsec.Parsec String () a -> Parsec.Parsec String () (Formula a)
parseEquiOrImp pp = do
  _ <- Parsec.char '('
  l <- parseFormula pp
  s <- Parsec.string "<=>" <|> Parsec.string "==>"
  r <- parseFormula pp
  _ <- Parsec.char ')'
  return $ (if s == "<=>" then Equi else Impl) l r

parseConjOrDisj :: Parsec.Parsec String () a -> Parsec.Parsec String () (Formula a)
parseConjOrDisj pp = do
  sign <- Parsec.string "conj" <|> Parsec.string "disj"
  fs <- parseList $ parseFormula pp
  return $ (if sign == "conj" then Conj else Disj) fs

parseForallOrExists :: Parsec.Parsec String () a -> Parsec.Parsec String () (Formula a)
parseForallOrExists pp = do
  s <- Parsec.string "A" <|> Parsec.string "E"
  var <- parseVar
  _ <- Parsec.char ' '
  f <- parseFormula pp
  return $ (if s == "A" then Forall else Exists) var f

-- parseVarFormulaUnsafe s = case parse parseVarFormula s of
--   Right f -> f
--   Left e -> error (show e)

-- >>> parse (parseFormula parseTerm) "(R[x,y]==>R[y])"
-- Right (R[x,y]==>R[y])
