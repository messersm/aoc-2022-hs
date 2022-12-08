module Main (main) where

import Aoc2022.Lib
import qualified Aoc2022.Puzzle1 as Puzzle1
import qualified Aoc2022.Puzzle2 as Puzzle2
import qualified Aoc2022.Puzzle3 as Puzzle3
import qualified Aoc2022.Puzzle4 as Puzzle4
import qualified Aoc2022.Puzzle5 as Puzzle5
import qualified Aoc2022.Puzzle6 as Puzzle6

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
  , the Puzzle3.part2
  , the Puzzle4.part1
  , the Puzzle4.part2
  , the Puzzle5.part1
  , the Puzzle5.part2
  , the Puzzle6.part1
  , the Puzzle6.part2
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
