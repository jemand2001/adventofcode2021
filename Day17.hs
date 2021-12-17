module Main where

import Utils
import Types
import Data.Maybe
import Data.List

type Velocity = Pair Int

minX, maxX, minY, maxY :: Int
(minX, maxX, minY, maxY) = (56, 76, -162,-134)
-- (minX, maxX, minY, maxY) = (20, 30, -10, -5)

target :: Area
target = P (P minX minY) (P maxX maxY)

simulate :: Point -> Velocity -> Maybe [Point]
simulate p@(P x y) v@(P dx dy)
  | x > maxX || y < minY = Nothing
  | p `inside` target = Just [p]
  | otherwise = (p:) <$> simulate (p+v) (P dx' (dy-1))
  where
    dx'
      | dx < 0 = dx+1
      | dx > 0 = dx-1
      | otherwise = 0

-- part1 :: [Int]
part1 = maximum $ map (maximum . map second) $ mapMaybe (simulate $ P 0 0) [P x y | x <- [0..76], y <- [-162..400]]
part2 = length $ mapMaybe (simulate $ P 0 0) [P x y | x <- [0 .. 76], y <- [-162 .. 400]]

main :: IO ()
main = print part2
