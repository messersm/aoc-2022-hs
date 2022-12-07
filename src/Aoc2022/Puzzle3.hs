module Aoc2022.Puzzle3 where

import Aoc2022.Lib (Puzzled(..))
import Aoc2022.Parsers

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

items :: ReadP String
items = many1 $ satisfy isLetter

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = error $ "Can't handle letter: " ++ [c]

-- | The item present in both compartments.
inboth :: String -> Char
inboth xs = head $ x `intersect` y
  where
    (x, y) = splitAt (length xs `div` 2) xs

part1 :: Puzzled [String] Int
part1 = Puzzled 3 1 (sepBy items newline) (sum . map (priority . inboth))
