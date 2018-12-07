import Data.Char
import Data.Ord
import Data.List

formula = readFile "inputs/day5.txt"

compat a b = a `isLowerOf` b || b `isLowerOf` a
  where isLowerOf a b = toLower b == a && isUpper b

react [] = []
react [x] = [x]
react letters = do
  let (x, y) = head $ zip letters rest
  if x `compat` y
    then react $ drop 2 letters
    else x : react rest
  where rest = tail letters

untilStable func items
  | result == items = items
  | otherwise = untilStable func result
  where result = func items

stableLength = length . untilStable react

filterByChar pool char = filter (\target -> toLower target /= toLower char) pool

main = do
  input <- formula
  let part1 = length $ untilStable react input
  return . minimum $ map (stableLength . filterByChar input) ['a'..'z']
