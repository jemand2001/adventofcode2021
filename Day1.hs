module Main where

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs $ tail xs

countIncreases :: [(Int, Int)] -> Int
countIncreases = sum . map (fromEnum . uncurry (<))
--                         (\(a, b) -> fromEnum (a < b))

sumTriples :: [Int] -> [Int]
sumTriples [] = []
sumTriples l@(_:xs) = sum (take 3 l) : sumTriples xs

part1 :: String -> String
part1 = (++ "\n") . show . countIncreases . pairwise . map read . lines

part2 :: String -> String
part2 = (++ "\n") . show . countIncreases . pairwise . sumTriples . map read . lines

main :: IO ()
main = interact part2
