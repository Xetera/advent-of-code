import Text.ParserCombinators.ReadP

data Rectangle = Rectangle Int Int Int Int Int deriving (Show)

parse :: String -> Rectangle
parse = fst . head . readP_to_S parser
parseInt :: ReadP Int
parseInt = readS_to_P reads
parser :: ReadP Rectangle
parser = do
  char '#'
  id <- parseInt
  skipSpaces
  char '@'
  skipSpaces
  x <- parseInt
  char ','
  y <- parseInt
  char ':'
  skipSpaces
  w <- parseInt
  char 'x'
  h <- parseInt
  return $ Rectangle id x y w h

file = lines <$> readFile "inputs/day3.txt"

-- main = do
