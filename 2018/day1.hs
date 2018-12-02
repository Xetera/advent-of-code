import qualified Data.IntSet as IntSet

processItems = foldr (+) 0

replace ('+':num) = num
replace any = any

-- This is INSTANT god bless IntSet
findRepeats (seen, current) nums = do
  let item = current + head nums
  let state = (item `IntSet.insert` seen, item)
  let nextNums = tail nums
  if item `IntSet.member` seen 
    then item : findRepeats state nextNums
    else findRepeats state nextNums

main = do 
  strings <- map replace . lines <$> readFile "inputs/day1.txt"
  let numbers = map (\n -> read n :: Int) $ strings
  let solution1 = processItems numbers

  let dupes = findRepeats (IntSet.fromList [], 0) (cycle numbers)
  return $ take 100000 $ dupes

