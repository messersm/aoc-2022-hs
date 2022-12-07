module Main (main) where

import Aoc2022.Lib
import Aoc2022.Puzzle1 as Puzzle1
import Aoc2022.Puzzle2 as Puzzle2
import Aoc2022.Puzzle3 as Puzzle3

import Options.Applicative
import Text.Printf

import Data.Semigroup ((<>))
import Numeric.Natural (Natural)

data Opts = Opts !Natural !Natural !String deriving (Show)

puzzles :: [Puzzle]
puzzles =
  [ the Puzzle1.part1
  , the Puzzle1.part2
  , the Puzzle2.part1
  , the Puzzle2.part2
  , the Puzzle3.part1
  ]

optsParser :: Parser Opts
optsParser =
  Opts <$>
      argument auto (metavar "puzzle")
  <*> argument auto (metavar "part")
  <*> argument str (metavar "inputfile")

main :: IO ()
main = do
  opts <- execParser $ info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Advent of code 2022 in Haskell")
  run opts

run :: Opts -> IO ()
run (Opts n p filename)
    | null matches = printf "No such puzzle: %d.%d\n" n p
    | otherwise    = do
      input <- readFile filename
      let result = solve puzzle input
      case result of
        Left error   -> putStrLn error
        Right result -> putStrLn result
  where
    matches = [puz | puz <- puzzles, num puz == n, part puz == p]
    puzzle = head matches
