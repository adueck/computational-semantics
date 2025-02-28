module ParseTools where

import Data.Functor.Identity
import Text.Parsec qualified as Parsec

parse :: (Parsec.Stream s Data.Functor.Identity.Identity t, Show t) => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse (rule <* Parsec.eof) "(source)"