module Aoc2022.Puzzle1 where

import Aoc2022.Lib (runParser, Puzzled(..))

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

newline :: ReadP ()
newline = do
  satisfy (== '\n')
  return ()

integer :: ReadP Integer
integer = read <$> (many1 $ satisfy isDigit)

parser :: ReadP [[Integer]]
parser = sepBy (sepBy1 integer newline) (count 2 newline)

part1 :: Puzzled [[Integer]] Integer
part1 = Puzzled 1 1 parser (\xs -> maximum $ sum <$> xs)

part2 :: Puzzled [[Integer]] Integer
part2 = Puzzled 1 2 parser (\xs -> sum $ take 3 $ reverse $ sort $ sum <$> xs)

