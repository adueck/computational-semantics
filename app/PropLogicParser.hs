module PropLogicParser
  ( parse,
    parseForm,
    parseFormUnsafe,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor.Identity
import PropLogicTypes
import Text.Parsec qualified as Parsec

parse :: (Parsec.Stream s Data.Functor.Identity.Identity t) => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

parseFormUnsafe :: String -> Form
parseFormUnsafe s = case parse parseForm s of
  Right f -> f
  Left e -> error (show e)

parseP :: Parsec.Parsec String () Form
parseP = do
  letter <- Parsec.oneOf "pqr"
  digits <- Parsec.many Parsec.digit
  return (P (letter : digits))

parseNg :: Parsec.Parsec String () Form
parseNg = do
  _ <- Parsec.char '-'
  Ng <$> parseForm

parseCnj :: Parsec.Parsec String () Form
parseCnj = do
  _ <- Parsec.char '&'
  Cnj <$> parseForms

parseForms :: Parsec.Parsec String () [Form]
parseForms = do
  _ <- Parsec.char '['
  f <- Parsec.sepBy parseForm (Parsec.spaces >> Parsec.char ',' >> Parsec.spaces)
  _ <- Parsec.char ']'
  return f

parseDsj :: Parsec.Parsec String () Form
parseDsj = do
  _ <- Parsec.char 'v'
  Dsj <$> parseForms

parseForm :: Parsec.Parsec String () Form
parseForm = parseP <|> parseNg <|> parseCnj <|> parseDsj

-- >>> parse parseForm "&[p,q2,-&[r1,-q]]"
-- Right &[p,q2,-&[r1,-q]]
