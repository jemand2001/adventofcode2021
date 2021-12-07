module Main where

import Data.List.Split (wordsBy)
import qualified Data.Map.Strict as M
import Types (Pair (..), fromList)
import Utils

type Point = Pair Int

type Line = Pair Point

type Grid = M.Map (Pair Int) Int

addLine :: [Int] -> [Int] -> Grid -> Grid
addLine xRange yRange m =
  foldr (\point -> M.insertWith (+) point 1) m (take (maximum $ length <$> P xRange yRange) $ zipWith P (cycle xRange) (cycle yRange))

populate :: [Line] -> Grid
populate = foldr addLine' M.empty
  where
    addLine' (P (P x1 y1) (P x2 y2)) m
      | x1 == x2 || y1 == y2 = addLine [min x1 x2 .. max x1 x2] [min y1 y2 .. max y1 y2] m
      | otherwise = m

populate' :: [Line] -> Grid
populate' = foldr addLine' M.empty
  where
    addLine' (P (P x1 y1) (P x2 y2))
      | x1 == x2 || y1 == y2 || (x1 < x2 && y1 < y2) || (x1 > x2 && y1 > y2) =
        addLine [min x1 x2 .. max x1 x2] [min y1 y2 .. max y1 y2]
      | otherwise = addLine [min x1 x2 .. max x1 x2] $ reverse [min y1 y2 .. max y1 y2]

parseLine :: String -> Line
parseLine s = parseLine' $ words s
  where
    parseLine' [p1, "->", p2] = parsePoint <$> P p1 p2
    parsePoint s = read <$> fromList (wordsBy (== ',') s)

part1 :: String -> String
part1 = evaluate (map parseLine . lines) (length . filter ((> 1) . snd) . M.toList . populate)

part2 :: String -> String
part2 = evaluate (map parseLine . lines) (length . filter ((> 1) . snd) . M.toList . populate)

diagnostic :: String -> String
diagnostic = (++ "\n") . show . (\xs -> P (minimum $ map first xs, maximum $ map first xs) (minimum $ map second xs, maximum $ map second xs)) . map fst . M.toList . populate' . map parseLine . lines

main :: IO ()
main = interact diagnostic
