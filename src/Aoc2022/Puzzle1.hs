{-# LANGUAGE LambdaCase #-}

module Aoc2022.Puzzle1 where

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


runParser :: ReadP a -> String -> Maybe a
runParser p s = if null result then Nothing else Just $ fst $ last result
  where
    result = readP_to_S p s

solve1 :: String -> String
solve1 input = case runParser parser input of
  Nothing -> "Invalid input"
  Just xs -> show $ maximum $ sum <$> xs

solve2 :: String -> String
solve2 input = case runParser parser input of
  Nothing -> "Invalid input"
  Just xs -> show $ sum $ take 3 $ reverse $ sort $ sum <$> xs
