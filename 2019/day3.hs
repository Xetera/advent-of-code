{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (isDigit)
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

type Coordinate = (Int, Int)

data Direction
  = L
  | R
  | U
  | D
  deriving (Show, Eq)

data Movement =
  Movement
    { direction :: Direction
    , amount :: Int
    }
  deriving (Show)

makeDirection :: Char -> Direction
makeDirection 'R' = R
makeDirection 'L' = L
makeDirection 'U' = U
makeDirection 'D' = D
makeDirection err = error $ "got invalid direction: " <> [err]

movement :: ReadP Movement
movement = do
  direction <- makeDirection <$> choice (map char "LRUD")
  amount <- read <$> many1 (satisfy isDigit)
  pure $ Movement {direction, amount}

breakMovement :: Movement -> [Movement]
breakMovement Movement {direction, amount} =
  replicate (amount - 1) (Movement {direction, amount = 1})

move :: Coordinate -> Movement -> Coordinate
move (x, y) Movement {amount, direction} =
  case direction of
    L -> (x + amount, y)
    R -> (x - amount, y)
    U -> (x, y + amount)
    D -> (x, y - amount)

moveAll :: Coordinate -> Movement -> [Coordinate]
moveAll c m = scanl move c $ breakMovement m

path :: String -> [Movement]
path = fst . last . readP_to_S (movement `sepBy1` char ',')

toCoordinates :: [Movement] -> [Coordinate]
toCoordinates = go (0, 0)
  where
    go _ [] = []
    go coord (x:xs) = moveAll coord x ++ go (move coord x) xs

distance :: Coordinate -> Int
distance (x, y) = abs x + abs y

closest :: Coordinate -> Coordinate -> Ordering
closest = compare `on` distance

intersectionSteps :: [Coordinate] -> [Coordinate] -> Coordinate -> Int
intersectionSteps a b intersection =
  let solve = fromJust . elemIndex intersection
   in solve a + solve b + 2

main :: IO ()
main = do
  [coord1, coord2] <-
    map (filter (/= (0, 0)) . toCoordinates . path) . lines <$>
    readFile "inputs/day3.txt"
  let intersections = (S.intersection `on` S.fromList) coord1 coord2
  let answer = minimumBy closest intersections
  -- part 1 solution
  print $ distance answer
  -- part 2 solution
  let answer2 = minimum $ S.map (intersectionSteps coord1 coord2) intersections
  print $ answer2
