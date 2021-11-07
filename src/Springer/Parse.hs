-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-ParserCombinators-ReadP.html
module Springer.Parse (module Text.ParserCombinators.ReadP, parse, parseWith, string_) where

import Control.Monad (void)
import Data.Char (isSpace)
import Text.ParserCombinators.ReadP

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

parseWith :: ReadP a -> String -> a
parseWith p s = case [a | (a, t) <- parse p s, all isSpace t] of
  [a] -> a
  [] -> error "no parse"
  _ -> error "ambiguous parse"

string_ :: String -> ReadP ()
string_ = void . string
