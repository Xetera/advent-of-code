import Data.Char (isDigit)
import Data.List as L
import Data.Vector as V
import Text.ParserCombinators.ReadP

nextCodes :: Vector Int -> Int -> (Int, Int, Int)
nextCodes vector index =
  (vector ! (index + 1), vector ! (index + 2), vector ! (index + 3))

replaceAt :: (Int -> Int -> Int) -> Vector Int -> Int -> Vector Int
replaceAt f vector index =
  let (first, second, third) = nextCodes vector index
   in vector // [(third, f (vector ! first) (vector ! second))]

runOpcode :: Int -> Vector Int -> Int -> Vector Int
runOpcode 1 = replaceAt (+)
runOpcode 2 = replaceAt (*)
runOpcode _ = error "you messed up"

process :: Int -> V.Vector Int -> Vector Int
process index nums
  | nums ! index == 99 = nums
  | otherwise = process (index + 4) newVector
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

answer :: V.Vector Int -> Int
answer = V.head

main :: IO ()
main = do
  numbersStr <- readFile "inputs/day2.txt"
  let vector = opcodes numbersStr
  let run = process 0
  print $ Prelude.length choices
  -- part 1 answer
  print $ answer $ run $ prepare vector (12, 2)
  let day2Target = 19690720
  let pairs = L.find ((== day2Target) . answer . run . prepare vector) choices
  case pairs of
    Just (x, y) -> print $ 100 * x + y
    Nothing -> print "No pairs found"
