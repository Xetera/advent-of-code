numsIO = map (read :: String -> Int) . lines <$> readFile "inputs/day1.txt"

pairs = zip <*> tail

main = do
  nums <- numsIO
  let numPairs = pairs nums
  let increasing = length $ filter (\(a, b) -> b > a) numPairs
  putStrLn (show $ increasing)
  let slidingWindow = map (\(a, b, c) -> a + b + c) $ zip3 (nums) (tail nums) (tail $ tail nums)
  let twoIncreasing = length $ filter (\(a, b) -> b > a) $ pairs slidingWindow
  putStrLn (show $ twoIncreasing)
