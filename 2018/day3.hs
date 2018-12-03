data Rect = Int Int Int Int Int

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim

toRect ('#':num:' ':'@':' ':rest) = Rect num 
  where

    
file = lines <$> readFile "inputs/day3.txt"

-- main = do
