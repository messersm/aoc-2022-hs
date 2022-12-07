-- | Shared parsers
module Aoc2022.Parsers where

import Text.ParserCombinators.ReadP

newline :: ReadP Char
newline = satisfy (== '\n')
