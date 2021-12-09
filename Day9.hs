module Main where

import Debug.Trace
import Utils
import Types
import Data.List

at :: [[a]] -> Pair Int -> a
at xss (P x y) = xss !! y !! x

findLowPoints :: [[Int]] -> [((Int, Int), Int)]
findLowPoints = findLowPoints' . zipWith (\y l -> zipWith (\x k -> ((x, y), k)) [0 ..] l) [0 ..]
  where
    findLowPoints' xss =
      concat
        [ [ t | t@((x, y), k) <- xs, x == 0 || snd (xs !! (x - 1)) > k, x == lenX || snd (xs !! (x + 1)) > k, y == 0 || snd (xss !! (y - 1) !! x) > k, y == lenY || snd (xss !! (y + 1) !! x) > k
          ]
          | xs <- xss
        ]
      where
        lenY = length xss - 1
        lenX = length (head xss) - 1

findBasinAreas :: [[Int]] -> [Integer]
findBasinAreas xss = map (fromIntegral . length . (\p -> floodFill [p] []) . uncurry P . fst) $ findLowPoints xss
  where
    lenY = length xss
    lenX = length $ head xss
    floodFill [] marked = marked
    floodFill (node:stack) marked
      | inside node = floodFill (n:e:s:w:stack) (node:marked)
      | otherwise = floodFill stack marked
      where
        inside node@(P x y)
          | x < 0 || x >= lenX || y < 0 || y >= lenY = False
          | node `elem` marked = False
          | xss `at` node == 9 = False
          | otherwise = True
        n = node - P 0 1
        e = node + P 1 0
        s = node + P 0 1
        w = node - P 1 0

threeHighest :: Ord a => [a] -> [a]
threeHighest xs = [a, b, c]
  where
    a = maximum xs
    xs' = delete a xs
    b = maximum xs'
    xs'' = delete b xs'
    c = maximum xs''

part1 :: String -> String
part1 = evaluate (map (map (read . (: []))) . lines) (sum . map ((+ 1) . snd) . findLowPoints)

part2 :: String -> String
part2 = evaluate (map (map (read . (: []))) . lines) (product . threeHighest . findBasinAreas)

main :: IO ()
main = interact part2
