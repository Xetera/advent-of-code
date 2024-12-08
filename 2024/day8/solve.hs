{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Monad (forM_, join)
import Control.Monad.IO.Class (MonadIO)
import Data.Attoparsec.Text qualified as P
import Data.Char (isAlphaNum)
import Data.Functor
import Data.List (sort, tails)
import Data.Map (Map, elems, insertWith)
import Data.Massiv.Array (Ix2 ((:.)))
import Data.Massiv.Array qualified as M
import Data.Massiv.Core (PrimMonad)
import Data.Massiv.Core.Index as I
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Debug.Trace (traceShowId)

data Cell = Beacon Char | Empty | Antinode deriving (Eq, Ord)

showText :: Cell -> T.Text
showText = \case
  Beacon c -> fromString [c]
  Antinode -> "#"
  Empty -> "."

instance Show Cell where
  show = T.unpack . showText

instance Semigroup Cell where
  Empty <> a = a
  a <> Empty = a
  a <> b = b

instance Monoid Cell where
  mempty = Empty

type Matrix = M.Array M.BL Ix2 Cell

toString :: Matrix -> T.Text
toString m = T.unlines (mconcat . fmap showText <$> M.toLists m)

cell :: P.Parser Cell
cell = do
  (P.char '.' $> Empty) <|> (Beacon <$> P.satisfy isAlphaNum)

parser = do
  out <- P.sepBy1 (P.many1 cell) (P.char '\n')
  P.endOfInput
  pure out

type PointMap = Map Cell [Ix2]

gatherIndexes :: Matrix -> PointMap
gatherIndexes = M.ifoldrS appendIndex mempty
  where
    appendIndex :: Ix2 -> Cell -> PointMap -> PointMap
    appendIndex ix cell map =
      case cell of
        Empty -> map
        beacon -> insertWith (<>) beacon [ix] map

computePairs :: [Ix2] -> [(Ix2, Ix2)]
computePairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

extremes :: (Ix2, Ix2) -> (Ix2, Ix2)
extremes (x, y) =
  let g = a - b
   in (a + g, b - g)
  where
    [a, b] = sort [x, y]

extremes2 :: (Ix2, Ix2) -> [Ix2]
extremes2 (x, y) =
  let g = a - b
   in take 1000 (iterate (+ g) a) ++ take 1000 (iterate (g `subtract`) b)
  where
    [a, b] = sort [x, y]

withinBounds :: Int -> Ix2 -> Bool
withinBounds dim (a :. b) = (a < dim) && (b < dim)

showAntinodes :: Matrix -> [Ix2] -> IO T.Text
showAntinodes m points = do
  a <- M.thaw m
  forM_ points $
    M.modify_ a $
      const (pure Antinode)
  n <- M.freeze M.Par a
  return (toString n)

process :: [Ix2] -> [Ix2]
process points =
  let pairs = computePairs points
   in pairs >>= (pairToList . extremes)
  where
    pairToList (a, b) = [a, b]

process2 :: [Ix2] -> [Ix2]
process2 points =
  let pairs = computePairs points
   in pairs >>= extremes2

main :: IO ()
main = do
  input <- TIO.readFile "input.txt"
  let matrix = case P.parseOnly parser input of
        Right out -> (M.fromLists' M.Par out :: Matrix)
        Left out -> error "whoops"
  let indexes = gatherIndexes matrix
  let pairs = process <$> elems indexes
  let pairs2 = process2 <$> elems indexes
  antinodes <- showAntinodes matrix (join pairs)
  antinodes2 <- showAntinodes matrix (join pairs2)
  putStrLn $ "part1=" ++ show (T.count "#" antinodes)
  putStrLn $ "part2=" ++ show (T.count "#" antinodes2)
