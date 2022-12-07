-- | Shared parsers
module Aoc2022.Parsers where

import Data.Char
import Text.ParserCombinators.ReadP

integer :: ReadP Integer
integer = read <$> (many1 $ satisfy isDigit)

newline :: ReadP Char
newline = satisfy (== '\n')
