import Data.List
import Debug.Trace 

processItems = foldr (+) 0

replace ('+':num) = num
replace any = any

-- This shit literally took over 2 minutes to find the number
findRepeats :: ([Int], Int) -> [Int] -> [Int]
findRepeats (seen, current) nums = do
  let item = current + head nums
  if item `elem` seen 
    then item : findRepeats (item:seen, item) (tail nums)
    else findRepeats (item:seen, item) (tail nums)

main = do 
  strings <- map replace . lines <$> readFile "inputs/day1.txt"
  let numbers = map (\n -> read n :: Int) $ strings
  let solution1 = processItems numbers

  let dupe = head $ findRepeats ([], 0) (cycle numbers)
  return dupe
  -- head . fst dupes

