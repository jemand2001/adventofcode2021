module Main where

import Data.List
import Data.Ord
import Utils

align :: [Int] -> Int
align l = minimum $ map distances [minimum l .. maximum l]
  where
    distances x = sum $ map (abs . (x -)) l

align' :: [Int] -> Int
align' l = minimum $ map distances [minimum l .. maximum l]
  where
    distances x = sum $ map (sumTo . abs . (x -)) l
    sumTo x = (x + 1) * x `div` 2

part1 :: String -> String
part1 = evaluate readXs align

part2 :: String -> String
part2 = evaluate readXs align'

main :: IO ()
main = interact part2
