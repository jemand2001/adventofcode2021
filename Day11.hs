module Main where

import Data.Maybe
import Types
import Utils

step :: Grid Int -> (Int, Grid Int)
step g = step' g $ withDimensions False g
  where
    step' :: Grid Int -> Grid Bool -> (Int, Grid Int)
    step' g flashed
      | not changed = (countBy (== 0) g'' + c, g'')
      | otherwise = (countBy id flashed', g''')
      where
        g' = g + 1
        flashed' = (> 9) <$> g'
        g''' = foldr increase g' $ withCoordinates flashed'
        (c, g'') = step' g''' flashed'
        changed = flashed /= flashed'
    (P x y) = dimensions g
    increase :: (Pair Int, Bool) -> Grid Int -> Grid Int
    increase (coords, True) g = foldr increase' g directions
      where
        increase' dir g'
          | x' < 0 || x' > x || y' < 0 || y' > y = g'
          | otherwise = set coords' ((g `at` coords') + 1) g
          where
            coords'@(P x' y') = coords + dir
    increase (_, False) g = g

countFlashes :: Int -> Grid Int -> Int
countFlashes 0 _ = 0
countFlashes steps grid = flashes + countFlashes (steps - 1) grid'
  where
    (flashes, grid') = step grid

part1 :: String -> String
part1 = evaluate parseGrid (countFlashes 100)

main :: IO ()
main = interact part1
