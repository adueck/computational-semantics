{-# LANGUAGE GADTs #-}

module Main where

import PredLogicParser

main :: IO ()
main = print ((parse (parseFormula parseTerm)) "R[x]")
