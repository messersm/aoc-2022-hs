module Aoc2022.Puzzle8 where

import Aoc2022.Lib (Puzzled(..))
import Aoc2022.Parsers
import Text.ParserCombinators.ReadP

import Data.List (transpose)

parser :: ReadP [[Int]]
parser = sepBy (many1 digit) newline

-- | Check, if the tree at (x, y) is visible
--
-- Really inefficient - should be using an array instead.
visible :: Int -> Int -> [[Int]] -> Bool
visible x y ts = or
    [ f x $ ts !! y
    , f (width - x - 1) $ reverse $ ts !! y
    , f y $ transpose ts !! x
    , f (height - y - 1) $ reverse $ transpose ts !! x
    ]
  where
    width  = length $ ts !! 0
    height = length ts
    f i ts = all (< (ts !! i)) $ take i ts

part1 :: Puzzled [[Int]] Int
part1 = Puzzled 8 1 parser solve
  where
    solve ts = length
        [ (x, y)
        | x <- [0..width-1]
        , y <- [0..height-1]
        , visible x y ts
        ]
      where
        width  = length $ ts !! 0
        height = length ts
