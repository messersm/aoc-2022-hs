-- | Shared parsers
module Aoc2022.Parsers where

import Data.Char
import Text.ParserCombinators.ReadP

integer :: ReadP Integer
integer = read <$> (many1 $ satisfy isDigit)

-- | Parses a positive integral
integral :: (Read a, Integral a) => ReadP a
integral = read <$> (many1 $ satisfy isDigit)

newline :: ReadP Char
newline = satisfy (== '\n')
