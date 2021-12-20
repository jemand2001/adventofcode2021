module Main where

import Data.List.Split
import qualified Data.Set as S
import Types hiding (Point)
import Utils (countTrue, evaluate)

-- data Point = P Int Int Int
type Point = (Int, Int, Int)

add :: Point -> Point -> Point
add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

type ScannerResult = [Point]

type ScannerDifferences = [(Pair Point, S.Set Int)]

-- -- | these functions rotate the point clockwise around the axis
-- rotateX, rotateY, rotateZ :: Point -> Point
-- rotateX (x, y, z) = (x, z, -y)
-- rotateY (x, y, z) = (-z, y, x)
-- rotateZ (x, y, z) = (y, -x, z)

-- | returns the position of the second scanner relative to the first one, if they overlap by 12 beacons
matches :: ScannerDifferences -> ScannerDifferences -> Maybe Point
matches l1 l2
  | length matchingPoints < 12 = Nothing
  | otherwise = Just $ getOrigin matchingPoints
  where
    matchingPoints = [P p1 p2 | (p1, d1) <- l1, (p2, d2) <- l2, d1 == d2]

getOrigin :: [Pair (Pair Point)] -> Point
getOrigin [] = undefined
getOrigin [_] = undefined
-- (a, b) matches (c, d) and (e, f) matches (g, h)
getOrigin ((P (P a b) (P c d)):(P (P e f) (P g h)):_) = _a

parseScanner :: [String] -> ScannerResult
parseScanner (_ : lines) = map (read . (\s -> '(' : s ++ ")")) lines
parseScanner [] = undefined

part1 :: String -> String
part1 = evaluate (map parseScanner . wordsBy null . lines) id

main :: IO ()
main = interact part1
