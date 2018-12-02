import qualified Data.Map.Strict as Map (Map(..), insert, empty, lookup, filter, size)
import Data.List
import Data.Maybe

items = lines <$> readFile "inputs/day2.txt"

-- this could probably be made better with insertLookupWithKey
occuranceCount line = foldr count (Map.empty) (line)
  where count char map = case Map.lookup char map of 
          Just x -> Map.insert char (x + 1) map
          Nothing -> Map.insert char 1 map

hasFreq amount freq = Map.size (Map.filter (==amount) freq) > 0

differenceCount :: String -> String -> Int
differenceCount one two = length . filter (==False) $ zipWith (==) one two

findCommon one two = map fst . filter (\t -> fst t == snd t) $ zip one two

main = do
  freqs <- map occuranceCount <$> items
  ids <- items
  let twos = filter (hasFreq 2) freqs
  let threes = filter (hasFreq 3) freqs
  let part1 = length twos * length threes
  let (one:two:_) = filter (\one -> case find (\two -> differenceCount one two == 1) ids of
        Just _ -> True
        Nothing -> False
        ) ids
  return $ findCommon one two