module Main where

import Data.List.Split
import Utils

data Population = P Integer Integer Integer Integer Integer Integer Integer Integer Integer

simulate :: Int -> [Integer] -> Population
simulate x l = simulate' x $ P (count' 0) (count' 1) (count' 2) (count' 3) (count' 4) (count' 5) (count' 6) (count' 7) (count' 8)
  where
    count' = toInteger . count l
    simulate' 0 p = p
    simulate' x (P a b c d e f g h i) = simulate' (x - 1) $ P b c d e f g (h + a) i a

sumPop :: Population -> Integer
sumPop (P a b c d e f g h i) = a + b + c + d + e + f + g + h + i

part1 :: String -> String
part1 = (++ "\n") . show . sumPop . simulate 80 . map read . wordsBy (== ',')

part2 :: String -> String
part2 = (++ "\n") . show . sumPop . simulate 256 . map read . wordsBy (== ',')

main :: IO ()
main = interact part2
