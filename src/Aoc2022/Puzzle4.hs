module Aoc2022.Puzzle4 where

import Aoc2022.Lib (Puzzled(..))
import Aoc2022.Parsers

import Data.Char
import Data.List
import Data.List.Extra
import Text.ParserCombinators.ReadP

section :: ReadP [Integer]
section = do
  x <- integer
  satisfy (== '-')
  y <- integer
  return [x..y]

pair :: ReadP ([Integer], [Integer])
pair = do
  first <- section
  satisfy (== ',')
  second <- section
  return (first, second)

fullyOverlapping :: [Integer] -> [Integer] -> Bool
fullyOverlapping x y = x `isSubsequenceOf` y || y `isSubsequenceOf` x

overlapping :: [Integer] -> [Integer] -> Bool
overlapping x y = not $ null $ intersect x y

part1 :: Puzzled [([Integer], [Integer])] Int
part1 = Puzzled 4 1 (sepBy pair newline) solve
  where
    solve = length . filter (uncurry fullyOverlapping)

part2 :: Puzzled [([Integer], [Integer])] Int
part2 = Puzzled 4 2 (sepBy pair newline) (length . filter (uncurry overlapping))
