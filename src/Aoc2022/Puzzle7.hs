{-# LANGUAGE ApplicativeDo #-}

module Aoc2022.Puzzle7 where

import qualified Data.Map.Strict  as Map

import Aoc2022.Lib (Puzzled(..))
import Aoc2022.Parsers
import Data.Char
import Data.List
import Data.Tree
import Text.ParserCombinators.ReadP

import Data.Map.Strict (Map)

data Line
    = Ls
    | CdRoot
    | CdDot
    | Cd String
    | Dir String
    | File Int String
  deriving (Show)

data AFile = AFile Int String deriving (Show)

parser :: ReadP [Line]
parser = sepBy (choice [ls, cdroot, cddot, cd, dir, file]) newline
  where
    dirname  = many1 $ satisfy isAlphaNum
    filename = many1 $ satisfy isAlphaNum +++ satisfy (== '.')

    ls     = const Ls     <$> string "$ ls"
    cdroot = const CdRoot <$> string "$ cd /"
    cddot  = const CdDot  <$> string "$ cd .."
    cd     = Cd   <$> (string "$ cd " *> dirname)
    dir    = Dir  <$> (string "dir " *> dirname)
    file   = File <$> integral <*> (string " " *> filename)


data FS = FS [String] (Map [String] [AFile])

empty :: FS
empty = FS [] Map.empty

contents :: FS -> (Map [String] [AFile])
contents (FS _ m) = m

fsFind :: FS -> [String]
fsFind fs = do
  (cdir, afiles) <- Map.toList $ contents fs
  AFile _ name <- afiles
  return $ "/" ++ intercalate "/" (cdir ++ [name])

handle :: FS -> Line -> FS
handle fs@(FS cdir files) CdRoot     = FS [] files
handle fs@(FS cdir files) CdDot      = FS (init cdir) files
handle fs@(FS cdir files) (Cd d)     = FS (cdir ++ [d]) files
handle fs@(FS cdir files) Ls         = fs
handle fs@(FS cdir files) (Dir _)    = fs
handle fs@(FS cdir files) (File s n) = FS cdir files'
  where
    f afile Nothing       = Just [afile]
    f afile (Just afiles) = Just $ afile : afiles
    files' = Map.alter (f (AFile s n)) cdir files

part1 = Puzzled 7 1 parser (fsFind . foldl' handle empty)
