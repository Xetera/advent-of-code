import Data.Char (isDigit)
import Data.List as L
import Data.Vector as V
import Text.ParserCombinators.ReadP

replaceAt :: (Int -> Int -> Int) -> Vector Int -> Int -> Vector Int
replaceAt f vector index =
  vector // [(third, f (vector ! first) (vector ! second))]
  where
    nextCodes v idx = L.map (\i -> v ! (idx + i)) [1, 2, 3]
    [first, second, third] = nextCodes vector index

runOpcode :: Int -> Vector Int -> Int -> Vector Int
runOpcode 1 = replaceAt (+)
runOpcode 2 = replaceAt (*)
runOpcode _ = error "you messed up"

solve :: Int -> V.Vector Int -> Int
solve index nums
  | nums ! index == 99 = V.head nums
  | otherwise = solve (index + 4) newVector
  where
    opcode = nums ! index
    newVector = runOpcode opcode nums index

prepare :: V.Vector Int -> (Int, Int) -> V.Vector Int
prepare v (x, y) = v // [(1, x), (2, y)]

opcodes :: String -> Vector Int
opcodes = V.fromList . fst . L.last . readP_to_S (parseInt `sepBy1` char ',')
  where
    parseInt = read <$> many1 (satisfy isDigit)

choices :: [(Int, Int)]
choices = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

main :: IO ()
main = do
  numbersStr <- readFile "inputs/day2.txt"
  let vector = opcodes numbersStr
  let run = solve 0
  -- part 1 answer
  print . run $ prepare vector (12, 2)
  -- part 2 answer
  let part2Target = 19690720
  let pairs = L.find ((== part2Target) . run . prepare vector) choices
  case pairs of
    Just (x, y) -> print $ 100 * x + y
    Nothing -> print "No pairs found"
