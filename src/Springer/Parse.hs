module Springer.Parse (module Text.ParserCombinators.ReadP, parse, parseWith) where

import Data.Char (isSpace)
import Text.ParserCombinators.ReadP

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

parseWith :: ReadP a -> String -> a
parseWith p s = case [a | (a, t) <- parse p s, all isSpace t] of
  [a] -> a
  [] -> error "no parse"
  _ -> error "ambiguous parse"
