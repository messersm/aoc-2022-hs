{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

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

data Part1 = Part1
data Part2 = Part2

instance Puzzled Part1 [[Integer]] Integer where
  num'    _    = 1
  part'   _    = 1
  parser' _    = parser
  solve'  _ xs =  maximum $ sum <$> xs

instance Puzzled Part2 [[Integer]] Integer where
  num'    _    = 1
  part'   _    = 2
  parser' _    = parser
  solve'  _ xs = sum $ take 3 $ reverse $ sort $ sum <$> xs
