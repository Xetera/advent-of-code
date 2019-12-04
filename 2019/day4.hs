import Data.Functor.Contravariant
import Data.List

paired :: String -> [(Char, Char)]
paired = zip <*> tail

passwordSpecs :: [String -> Bool]
passwordSpecs = [isSix, isIncreasing]
  where
    isSix str = length str == 6
    isIncreasing = all (uncurry (<=)) . paired

isValidWith :: (String -> Bool) -> String -> Bool
isValidWith comparison =
  getPredicate . foldMap Predicate $ comparison : passwordSpecs

part1 :: String -> Bool
part1 = isValidWith $ any (uncurry (==)) . paired

part2 :: String -> Bool
part2 = isValidWith $ any ((== 2) . length) . group

main :: IO ()
main = do
  let (low, high) = (235741, 706948)
  let range = map show [low .. high]
  let solve f = print . length $ filter f range
  -- part 1
  solve part1
  -- part 2
  solve part2
