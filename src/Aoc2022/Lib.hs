module Aoc2022.Lib where

import Text.ParserCombinators.ReadP

runParser :: ReadP a -> String -> Maybe a
runParser p s = if null result then Nothing else Just $ fst $ last result
  where
    result = readP_to_S p s


class Puzzle a where
  parser :: ReadP a
  solve  :: Show b => a -> b
