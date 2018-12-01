import qualified Data.IntSet as IntSet

processItems = foldr (+) 0

replace ('+':num) = num
replace any = any

-- This shit literally took over 2 minutes to find the number
findRepeats (seen, current) nums = do
  let item = current + head nums
  if item `IntSet.member` seen 
    then item : findRepeats (item `IntSet.insert` seen, item) (tail nums)
    else findRepeats (item `IntSet.insert` seen, item) (tail nums)

main = do 
  strings <- map replace . lines <$> readFile "inputs/day1.txt"
  let numbers = map (\n -> read n :: Int) $ strings
  let solution1 = processItems numbers

  let dupes = findRepeats (IntSet.fromList [], 0) (cycle numbers)
  return $ take 100 $ dupes

