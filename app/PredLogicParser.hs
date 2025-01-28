module PredLogicParser
  ( parse,
    parseFormula,
    parseFormulaUnsafe,
  )
where

import Control.Applicative
import Data.Functor.Identity
import PredLogicTypes
import Text.Parsec qualified as Parsec

parse :: (Parsec.Stream s Data.Functor.Identity.Identity t) => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

parseVar :: Parsec.Parsec String () Variable
parseVar = do
  letter <- Parsec.oneOf "xyz"
  digits <- Parsec.many Parsec.digit
  return (Variable (letter : digits) [])

parseFormula :: Parsec.Parsec String () (Formula Variable)
parseFormula = parseAtom <|> parseEq <|> parseNeg <|> parseEquiOrImp <|> parseConj <|> parseConj <|> parseDisj <|> parseForall <|> parseExists

parseAtom :: Parsec.Parsec String () (Formula Variable)
parseAtom = do
  _ <- Parsec.char 'R'
  vars <- parseVars
  return $ Atom "R" vars

parseEq :: Parsec.Parsec String () (Formula Variable)
parseEq = do
  l <- parseVar
  _ <- Parsec.string "=="
  r <- parseVar
  return $ Eq l r

parseNeg :: Parsec.Parsec String () (Formula Variable)
parseNeg = do
  _ <- Parsec.char '~'
  f <- parseFormula
  return $ Neg f

parseEquiOrImp :: Parsec.Parsec String () (Formula Variable)
parseEquiOrImp = do
  _ <- Parsec.char '('
  l <- parseFormula
  s <- Parsec.string "<=>" <|> Parsec.string "==>"
  r <- parseFormula
  _ <- Parsec.char ')'
  return $ if s == "<=>" then Equi l r else Impl l r

parseConj :: Parsec.Parsec String () (Formula Variable)
parseConj = do
  _ <- Parsec.string "conj"
  fs <- parseFormulas
  return $ Conj fs

parseDisj :: Parsec.Parsec String () (Formula Variable)
parseDisj = do
  _ <- Parsec.string "disj"
  fs <- parseFormulas
  return $ Disj fs

parseForall :: Parsec.Parsec String () (Formula Variable)
parseForall = do
  _ <- Parsec.string "A"
  var <- parseVar
  _ <- Parsec.char ' '
  f <- parseFormula
  return $ Forall var f

parseExists :: Parsec.Parsec String () (Formula Variable)
parseExists = do
  _ <- Parsec.string "E"
  var <- parseVar
  _ <- Parsec.char ' '
  f <- parseFormula
  return $ Exists var f

parseVars :: Parsec.Parsec String () [Variable]
parseVars = do
  _ <- Parsec.char '['
  f <- Parsec.sepBy parseVar (Parsec.spaces >> Parsec.char ',' >> Parsec.spaces)
  _ <- Parsec.char ']'
  return f

parseFormulas :: Parsec.Parsec String () [Formula Variable]
parseFormulas = do
  _ <- Parsec.char '['
  f <- Parsec.sepBy parseFormula (Parsec.spaces >> Parsec.char ',' >> Parsec.spaces)
  _ <- Parsec.char ']'
  return f

-- >>> parse parseFormula "Ax Ay (R[x,y]==>R[y,x])"
-- Right Ax Ay (R[x,y]==>R[y,x])

parseFormulaUnsafe :: String -> Formula Variable
parseFormulaUnsafe s = case parse parseFormula s of
  Right f -> f
  Left e -> error (show e)
