module Aoc2022.Lib where

import Text.ParserCombinators.ReadP
import Numeric.Natural

runParser :: ReadP a -> String -> Maybe a
runParser p s
    | null result = Nothing
    | otherwise   = Just $ fst $ last $ result
  where
    result = readP_to_S p s

data Puzzled a b = Puzzled
  { num'    :: !Natural
  , part'   :: !Natural
  , parser' :: !(ReadP a)
  , solve'  :: !(a -> b)
  }

data Puzzle = Puzzle
  { num   :: !Natural
  , part  :: !Natural
  , solve :: !(String -> Either String String)
  }

the :: (Show b) => Puzzled a b -> Puzzle
the p = Puzzle (num' p) (part' p) f
  where
    f s = case parsed of
        Nothing -> Left "Invalid input"
        Just x  -> Right $ show $ solve' p x
      where
        parsed = runParser (parser' p) s
