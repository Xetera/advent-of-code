import Data.Char (isDigit)
import Data.List
import Text.ParserCombinators.ReadP

parse :: String -> [Int]
parse input = fst . last $ readP_to_S (parseInt `sepBy` (char '-')) input
  where
    parseInt = read <$> many1 (satisfy isDigit)

dataM :: IO String
dataM = readFile "inputs/day4.txt"

paired :: String -> [(Char, Char)]
paired = zip <*> tail

passwordSpecs :: [String -> Bool]
passwordSpecs = [isSix, isIncreasing]
  where
    isSix = (== 6) . length
    isIncreasing = all (uncurry (<=)) . paired

isValidWith :: (String -> Bool) -> String -> Bool
isValidWith comparison str = all ($ str) (comparison : passwordSpecs)

part1 :: String -> Bool
part1 = isValidWith $ any (uncurry (==)) . paired

part2 :: String -> Bool
part2 = isValidWith $ any ((== 2) . length) . group

main :: IO ()
main = do
  [low, high] <- parse <$> dataM
  let range = map show [low .. high]
  let solve f = print . length $ filter f range
  -- part 1
  solve part1
  -- part 2
  solve part2
