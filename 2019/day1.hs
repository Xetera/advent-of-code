dataM :: IO [Int]
dataM = map read . lines <$> readFile "inputs/day1.txt"

calculate :: Int -> Int
calculate num = floor (fromIntegral num / 3) - 2

calculateAll :: Int -> Int
calculateAll fuel = sum . takeWhile (> 0) . tail $ iterate calculate fuel

main :: IO ()
main = do
  masses <- dataM
  -- part 1
  print $ sum $ map calculate masses
  -- part 2
  print $ sum $ map calculateAll masses
