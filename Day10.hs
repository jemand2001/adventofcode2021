module Main where

import Utils
import Data.List (sort)
import Debug.Trace
import Data.Either (isLeft, isRight)

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right a) = a

errorScore :: Char -> Integer
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137
errorScore _ = undefined

autocompleteScore :: Char -> Integer -> Integer
autocompleteScore ')' = (+ 1) . (* 5)
autocompleteScore ']' = (+ 2) . (* 5)
autocompleteScore '}' = (+ 3) . (* 5)
autocompleteScore '>' = (+ 4) . (* 5)
autocompleteScore _ = undefined

findError :: [Char] -> String -> Either Integer Integer
findError stack "" = Right $ foldl (flip autocompleteScore) 0 stack
findError stack ('[' : s) = findError (']' : stack) s
findError stack ('{' : s) = findError ('}' : stack) s
findError stack ('(' : s) = findError (')' : stack) s
findError stack ('<' : s) = findError ('>' : stack) s
findError (c : stack) (c' : s)
  | c == c' = findError stack s
  | otherwise = Left $ errorScore c'
findError _ _ = undefined

part1 :: String -> String
part1 = evaluate lines (sum . map fromLeft . filter isLeft . map (findError []))

part2 :: String -> String
part2 = evaluate lines (middle . sort . filter (/= 0) . map fromRight . filter isRight . map (findError []))
  where middle xs = xs !! (length xs `div` 2)

main :: IO ()
main = interact part1
