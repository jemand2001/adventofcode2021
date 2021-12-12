module Main where

import Debug.Trace
import Utils
import Types
import Data.List

findLowPoints :: Grid Int -> [((Int, Int), Int)]
findLowPoints (G xss) = findLowPoints' . zipWith (\y l -> zipWith (\x k -> ((x, y), k)) [0 ..] l) [0 ..] $ xss
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

findBasinAreas :: Grid Int -> [Integer]
findBasinAreas g@(G xss) = map (fromIntegral . length . (\p -> floodFill [p] []) . uncurry P . fst) $ findLowPoints g
  where
    lenY = length xss
    lenX = length $ head xss
    floodFill [] marked = marked
    floodFill (node:stack) marked
      | inside node = floodFill (node + north:node + east:node + south:node + west:stack) (node:marked)
      | otherwise = floodFill stack marked
      where
        inside node@(P x y)
          | x < 0 || x >= lenX || y < 0 || y >= lenY = False
          | node `elem` marked = False
          | g `at` node == 9 = False
          | otherwise = True

threeHighest :: Ord a => [a] -> [a]
threeHighest xs = [a, b, c]
  where
    a = maximum xs
    xs' = delete a xs
    b = maximum xs'
    xs'' = delete b xs'
    c = maximum xs''

part1 :: String -> String
part1 = evaluate parseGrid (sum . map ((+ 1) . snd) . findLowPoints)

part2 :: String -> String
part2 = evaluate parseGrid (product . threeHighest . findBasinAreas)

main :: IO ()
main = interact part2
