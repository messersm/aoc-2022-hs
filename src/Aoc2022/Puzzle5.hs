module Aoc2022.Puzzle5 where

import Aoc2022.Lib (Puzzled(..))
import Aoc2022.Parsers

import Data.Char
import Data.List       as List
import Data.List.Extra as List
import Data.Map.Strict
import Data.Maybe
import Text.ParserCombinators.ReadP

import Debug.Trace

data Move = Move !Int !Int !Int deriving (Show)

row :: ReadP [Maybe Char]
row = sepBy1 (cell +++ empty) (satisfy (== ' '))
  where
    empty = do
      string "   "
      return Nothing
    cell  = do
      satisfy (== '[')
      c <- satisfy isLetter
      satisfy (== ']')
      return $ Just c

footer :: ReadP [Int]
footer = do
  skipSpaces
  xs <- sepBy1 integral skipSpaces
  skipSpaces
  return xs

move :: ReadP Move
move = do
  string "move "
  count <- integral
  string " from "
  from <- integral
  string " to "
  to <- integral
  return $ Move count from to

stacks :: ReadP (Map Int [Char])
stacks = do
  rows    <- sepBy1 row newline
  indices <- footer
  let stks = catMaybes <$> transpose rows
  return $ fromList $ zip indices stks

parser :: ReadP (Map Int [Char], [Move])
parser = do
  s <- stacks
  moves <- sepBy1 move newline
  return (s, moves)

apply :: Map Int [Char] -> Move -> Map Int [Char]
apply m (Move 0     _    _ ) = m
apply m (Move count from to) = apply m' $ Move (count-1) from to
  where
    source = m ! from
    m' = case uncons source of
      Nothing           -> m
      Just (x, source') -> f m
        where
          f = (adjust (\_ -> source') from) . (adjust (x :) to)

apply2 :: Map Int [Char] -> Move -> Map Int [Char]
apply2 m (Move count from to) = f m
  where
    source = m ! from
    (xs, source') = List.splitAt count source
    f = (adjust (\_ -> source') from) . (adjust (xs ++) to)

top :: Map Int [Char] -> [Char]
top = List.map head . elems

part1 :: Puzzled (Map Int [Char], [Move]) [Char]
part1 = Puzzled 5 1 parser solve
  where
    solve (m, moves) = top $ List.foldl' apply m moves

part2 :: Puzzled (Map Int [Char], [Move]) [Char]
part2 = Puzzled 5 2 parser solve
  where
    solve (m, moves) = top $List.foldl' apply2 m moves
