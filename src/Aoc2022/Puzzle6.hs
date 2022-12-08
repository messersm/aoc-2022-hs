module Aoc2022.Puzzle6 where

import Aoc2022.Lib (Puzzled(..))
import Aoc2022.Parsers

import Data.Char
import Data.List       as List
import Data.List.Extra as List
import Text.ParserCombinators.ReadP

text :: ReadP String
text = many get

slicesOf :: Int -> [a] -> [[a]]
slicesOf n xs = filter ((== n) . length) $ take n <$> tails xs

solve :: Int -> String -> Int
solve n xs = head [i+n | (i, s) <- zip [0..] $ slicesOf n xs, not $ anySame s]

part1 :: Puzzled String Int
part1 = Puzzled 6 1 text (solve 4)

part2 :: Puzzled String Int
part2 = Puzzled 6 2 text (solve 14)
