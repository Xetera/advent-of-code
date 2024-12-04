{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char
import Data.Functor
import Data.Monoid
import Text.ParserCombinators.ReadP

data Part = One | Two

data Action
  = Multiply (Product Int)
  | Start
  | Stop
  deriving (Show, Eq)

mulP :: ReadP Action
mulP = do
  _ <- string "mul"
  args <- betweenParens (integer `sepBy` char ',')
  return $ Multiply (mconcat args)
  where
    betweenParens = between (char '(') (char ')')
    integer = Product . read <$> many1 (satisfy isDigit)

actions :: ReadP (Maybe Action)
actions = (Just <$> (mulP <++ don'tP <++ doP)) <++ (get $> Nothing)
  where
    doP = string "do()" $> Start
    don'tP = string "don't()" $> Stop

parse :: String -> [Action]
parse input = case readP_to_S actions input of
  [(Just result, "")] -> [result]
  [(Just result, xs)] -> result : parse xs
  [(Nothing, xs)] -> parse xs
  _ -> []

solve :: Part -> [Action] -> Int
solve part actions = getSum . mconcat $ go actions
  where
    go = \case
      (Stop : xs) -> case part of
        One -> go xs
        Two -> go (dropWhile (/= Start) xs)
      (Start : xs) -> go xs
      (Multiply prod : xs) -> Sum (getProduct prod) : go xs
      [] -> mempty

main :: IO ()
main = do
  text <- readFile "input.txt"
  let actions = parse text
  print (solve One actions)
  print (solve Two actions)
