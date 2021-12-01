numsIO = map (read :: String -> Int) . lines <$> readFile "inputs/day1.txt"

pairs = zip <*> tail

filterIncreasing :: [(Int, Int)] -> [(Int, Int)]
filterIncreasing = filter (\(a, b) -> b > a)

main = do
  nums <- numsIO
  let numPairs = pairs nums
  let increasing = length $ filterIncreasing numPairs
  print increasing
  let slidingWindow = map (\(a, b, c) -> a + b + c) $ zip3 (nums) (tail nums) (tail $ tail nums)
  let twoIncreasing = length . filterIncreasing $ pairs slidingWindow
  print twoIncreasing
