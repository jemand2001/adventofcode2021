module Main where

import Data.List
import Data.List.Split
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

golf1 :: String -> String
golf1 = (++"\n").show.(\l->(l::[Int])!!(length l`div`2)).sort.map read.wordsBy(==',')

main :: IO ()
main = interact golf1
