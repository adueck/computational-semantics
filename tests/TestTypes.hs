module TestTypes where

data TestBlock where
  TestBlock :: (Show a, Eq b, Show b) => ([(a, b)], a -> b) -> TestBlock