{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Char
import Data.List (partition)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP

inputIO = lines <$> readFile "./inputs/day2.txt"

data Direction
  = Forward Int
  | Depth Int
  deriving (Show)

data RunningAim = RunningAim
  { aim :: Int,
    depth :: Int,
    forward :: Int
  }
  deriving (Show)

extractDepth = down <|> up <|> forward
  where
    readDigit = do
      skipSpaces
      digits <- many1 $ satisfy isDigit
      return $ read digits
    up = do
      string "up"
      digits <- readDigit
      return . Depth $ (-1 * digits)
    down = do
      string "down"
      skipSpaces
      digits <- readDigit
      return . Depth $ digits
    forward = do
      string "forward"
      digits <- readDigit
      return . Forward $ digits

predicate (Forward _) = True
predicate (Depth _) = False

val (Forward a) = a
val (Depth a) = a

main = do
  input <- inputIO
  let directions = concatMap (fmap fst . readP_to_S extractDepth) input
  let (forward, depth) = partition predicate directions
  let f = sum $ map val forward
  let d = sum $ map val depth
  print (f * d)
  let RunningAim {forward, depth} = foldl (flip aim) RunningAim {aim = 0, forward = 0, depth = 0} directions
  print (forward * depth)
  return ()
  where
    aim :: Direction -> RunningAim -> RunningAim
    aim (Forward num) r@RunningAim {aim, forward, depth} =
      r {forward = forward + num, depth = depth + (aim * num)}
    aim (Depth num) r@RunningAim {aim, depth} =
      r {aim = aim + num}
