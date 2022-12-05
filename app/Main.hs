module Main (main) where

import Aoc2022.Puzzle1 as Puzzle1

import Options.Applicative
import Text.Printf

import Data.Semigroup ((<>))
import Numeric.Natural (Natural)

data Opts = Opts !Natural !Natural !String deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts <$>
    option auto (
      short 'p'
      <> long "puzzle"
      <> metavar "Nat"
      <> value 1
      <> showDefault
      <> help "Number of the puzzle")
  <*> option auto (
      short 's'
      <> long "subpuzzle"
      <> metavar "Nat"
      <> value 1
      <> showDefault
      <> help "Number of the sub puzzle")
  <*> strOption (
      short 'i'
      <> long "input"
      <> help "Input filename")


main :: IO ()
main = do
  opts <- execParser $ info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Advent of code 2022 in Haskell")
  run opts

run :: Opts -> IO ()
run (Opts 1 1 filename) = do
  input <- readFile filename
  putStrLn $ Puzzle1.solve1 input
run (Opts 1 2 filename) = do
  input <- readFile filename
  putStrLn $ Puzzle1.solve2 input

run (Opts p s _) = printf "No such puzzle: %d.%d\n" p s
