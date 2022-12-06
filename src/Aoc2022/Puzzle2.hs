{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Aoc2022.Puzzle2 where

import Aoc2022.Lib (runParser, Puzzled(..))
import Data.List.Extra
import Text.ParserCombinators.ReadP

data Choice  = Rock | Paper | Scissors  deriving (Eq, Show, Enum, Bounded)
data Outcome = Win  | Loss  | Draw      deriving (Eq, Show, Enum)

class Scoreable a where
  score :: a -> Int

instance Scoreable Choice where
  score Rock     = 1
  score Paper    = 2
  score Scissors = 3

instance Scoreable Outcome where
  score Win  = 6
  score Loss = 0
  score Draw = 3

-- | The choice winning against the given value
winning :: Choice -> Choice
winning Rock     = Paper
winning Paper    = Scissors
winning Scissors = Rock

outcome :: Choice -> Choice -> Outcome
outcome x y
  | winning x == y = Win
  | winning y == x = Loss
  | otherwise      = Draw

beats :: Choice -> Choice -> Bool
x `beats` y = winning y == x

table :: [(Choice, Choice, Outcome)]
table = do
  first  <- enumerate
  second <- enumerate
  return (first, second, outcome first second)

total :: Choice -> Choice -> Int
total opp own = score own + (score $ outcome opp own)

opponent :: ReadP Choice
opponent = do
  char <- satisfy (`elem` "ABC")
  case char of
    'A' -> return Rock
    'B' -> return Paper
    'C' -> return Scissors
    _   -> pfail

own :: ReadP Choice
own = do
  char <- satisfy (`elem` "XYZ")
  case char of
    'X' -> return Rock
    'Y' -> return Paper
    'Z' -> return Scissors
    _   -> pfail

outcomeP :: ReadP Outcome
outcomeP = do
  char <- satisfy (`elem` "XYZ")
  case char of
    'X' -> return Loss
    'Y' -> return Draw
    'Z' -> return Win
    _   -> pfail -- never

parser1 :: ReadP [(Choice, Choice)]
parser1 = many $ do
  opp <- opponent
  skipSpaces
  mine <- own
  optional $ satisfy (== '\n')
  return (opp, mine)

parser2 :: ReadP [(Choice, Outcome)]
parser2 = many $ do
  opp <- opponent
  skipSpaces
  out <- outcomeP
  optional $ satisfy (== '\n')
  return (opp, out)

data Part1 = Part1
data Part2 = Part2

instance Puzzled Part1 [(Choice, Choice)] Int where
  num'    _    = 2
  part'   _    = 1
  parser' _    = parser1
  solve'  _ xs = sum $ uncurry total <$> xs

instance Puzzled Part2 [(Choice, Outcome)] Int where
  num'    _    = 2
  part'   _    = 2
  parser' _    = parser2
  solve'  _ xs = sum $ uncurry total <$> choices
    where
      choices = do
        (first, expected) <- xs
        let second = head [y | (x, y, z) <- table, x == first, z == expected]
        return (first, second)
