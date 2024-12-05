{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Char (isDigit)
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.PatriciaTree (UGr)
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Text.ParserCombinators.ReadP

type RawConstraint = (Int, Int)

data Puzzle = Puzzle
  { allRules :: UGr,
    pages :: V.Vector (V.Vector Int)
  }
  deriving (Show)

buildRules :: [RawConstraint] -> UGr
buildRules constraints = G.mkUGraph (constraints >>= \(a, b) -> [a, b]) constraints

int :: ReadP Int
int = read <$> munch1 isDigit

constraint :: ReadP (Int, Int)
constraint = do
  a <- int
  _ <- char '|'
  b <- int
  pure (a, b)

pageP :: ReadP (V.Vector Int)
pageP = V.fromList <$> int `sepBy1` char ','

constraintsP :: ReadP [RawConstraint]
constraintsP = constraint `sepBy1` char '\n'

puzzleP :: ReadP Puzzle
puzzleP = do
  constraints <- constraintsP
  _ <- skipSpaces
  pages <- V.fromList <$> pageP `sepBy1` char '\n'
  eof
  pure Puzzle {allRules = buildRules constraints, pages = pages}

readPuzzle text =
  case readP_to_S puzzleP text of
    [(result, "")] -> result
    _ -> error "Shouldn't happen"

extractRules :: UGr -> V.Vector Int -> UGr
extractRules graph nums = G.subgraph (V.toList nums) graph

delConnectingEdges :: Int -> UGr -> UGr
delConnectingEdges x graph = foldr (G.delEdge . G.toEdge) graph (G.out graph x)

isValid :: V.Vector Int -> UGr -> Bool
isValid nums graph
  | V.null nums = True
  | otherwise = case G.inn graph head of
      [] -> isValid (V.tail nums) $ delConnectingEdges head graph
      _ -> False
  where
    head = V.head nums

fixOrder :: V.Vector Int -> Int -> UGr -> V.Vector Int
fixOrder nums i graph
  | i >= V.length nums = nums
  | otherwise =
      case G.inn graph head of
        [] -> fixOrder nums (i + 1) $ delConnectingEdges head graph
        (x : _) ->
          let (val, _) = G.toEdge x
              (Just idx) = V.findIndex (== val) nums
              updated = (nums V.// [(idx, head), (i, val)])
           in fixOrder updated i graph
  where
    head = nums V.! i

findMiddle :: V.Vector Int -> Int
findMiddle x = x V.! (V.length x `div` 2)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let puzzle = readPuzzle input
      (passing, nonPassing) =
        V.partitionWith
          ( \pages ->
              let rules = extractRules puzzle.allRules pages
               in (if isValid pages rules then Left pages else Right (pages, rules))
          )
          puzzle.pages
      part1 = V.sum $ V.map findMiddle passing
      part2 = V.sum $ V.map (\(nums, rules) -> findMiddle $ fixOrder nums 0 rules) nonPassing
   in putStrLn $ mconcat ["part1:", show part1, "\n", "part2:", show part2]
