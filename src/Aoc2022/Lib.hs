{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aoc2022.Lib where

import Text.ParserCombinators.ReadP
import Numeric.Natural

runParser :: ReadP a -> String -> Maybe a
runParser p s
    | null result = Nothing
    | otherwise   = Just $ fst $ last $ result
  where
    result = readP_to_S p s

class Show b => Puzzled p a b | p -> a b where
  num'    :: p -> Natural
  part'   :: p -> Natural
  parser' :: p -> ReadP a
  solve'  :: p -> a -> b

data Puzzle = Puzzle
  { num  :: Natural
  , part :: Natural
  , solve :: String -> Either String String
  }

the :: Puzzled p a b => p -> Puzzle
the p = Puzzle (num' p) (part' p) f
  where
    f s = case parsed of
        Nothing -> Left "Invalid input"
        Just x  -> Right $ show $ solve' p x
      where
        parsed = runParser (parser' p) s

