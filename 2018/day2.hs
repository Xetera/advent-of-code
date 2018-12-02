import qualified Data.Map.Strict as Map (Map(..), insert, empty, lookup, filter, size)

items = lines <$> readFile "inputs/day2.txt"

-- this could probably be made better with insertLookupWithKey
occuranceCount line = foldr count (Map.empty) (line)
  where count char map = case Map.lookup char map of 
          Just x -> Map.insert char (x + 1) map
          Nothing -> Map.insert char 1 map

hasFreq amount freq = Map.size (Map.filter (==amount) freq) > 0

main = do
  freqs <- map occuranceCount <$> items
  let twos = filter (hasFreq 2) freqs
  let threes = filter (hasFreq 3) freqs
  return $ length twos * length threes
