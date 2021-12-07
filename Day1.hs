module Main where

import Utils

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs $ tail xs

countIncreases :: [(Int, Int)] -> Int
countIncreases = sum . map (fromEnum . uncurry (<))
--                         (\(a, b) -> fromEnum (a < b))

sumTriples :: [Int] -> [Int]
sumTriples [] = []
sumTriples l@(_:xs) = sum (take 3 l) : sumTriples xs

part1 :: String -> String
part1 = evaluate readXs (countIncreases . pairwise)

part2 :: String -> String
part2 = evaluate readXs (countIncreases . pairwise . sumTriples)

main :: IO ()
main = interact part2
