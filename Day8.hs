module Main where

import Data.List (group, groupBy, intercalate, intersect, nub)
import Data.List.Split
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Debug.Trace
import Utils

type Mapping = [(Char, String)]

unique :: Int -> Bool
unique = (`elem` [2, 3, 4, 7])

resolve :: [String] -> Mapping -> Mapping
resolve xs m = m' ++ findA ++ findC ++ findD ++ findG
  where
    m' = filter ((== 1) . length . snd) m
    findC = [(ch, "c") | (ch, s) <- m, s == "ac", S.fromList [ch, extract 'f'] `elem` sets]
    findA = [(ch, "a") | (ch, s) <- m, s == "ac", ch /= c]
    findG = [(ch, "g") | (ch, s) <- m, s == "dg", S.fromList [a, extract 'b', c, extract 'e', extract 'f', ch] `elem` sets]
    findD = [(ch, "d") | (ch, s) <- m, s == "dg", ch /= g]
    extract c = fst $ head $ filter ((== [c]) . snd) m
    a = fst $ head findA
    c = fst $ head findC
    g = fst $ head findG
    sets = map S.fromList xs

from7Segment :: String -> Char
from7Segment s
  | S.fromList s == S.fromList "abcefg" = '0'
  | S.fromList s == S.fromList "cf" = '1'
  | S.fromList s == S.fromList "acdeg" = '2'
  | S.fromList s == S.fromList "acdfg" = '3'
  | S.fromList s == S.fromList "bcdf" = '4'
  | S.fromList s == S.fromList "abdfg" = '5'
  | S.fromList s == S.fromList "abdefg" = '6'
  | S.fromList s == S.fromList "acf" = '7'
  | S.fromList s == S.fromList "abcdefg" = '8'
  | S.fromList s == S.fromList "abcdfg" = '9'
from7Segment _ = undefined

findMapping' :: [String] -> Mapping
findMapping' xs = resolve xs $ nub . findMapping'' . concat $ xs
  where
    findMapping'' xs =
      [ (fst $ fromJust $ lookupBy ((== 4) . snd) $ counts xs, "e"),
        (fst $ fromJust $ lookupBy ((== 6) . snd) $ counts xs, "b"),
        (fst $ fromJust $ lookupBy ((== 9) . snd) $ counts xs, "f")
      ]
        ++ [(ch, "ac") | (ch, ch') <- counts xs, ch' == 8]
        ++ [(ch, "dg") | (ch, ch') <- counts xs, ch' == 7]
    counts xs = map (\ch -> (ch, count xs ch)) xs

execMapping :: Mapping -> [String] -> Int
execMapping m s@[_, _, _, _] = read $ map (from7Segment . concatMap (fromJust . (`lookup` m))) s
execMapping _ _ = undefined

solve2 :: [[[String]]] -> [Int]
solve2 [] = []
solve2 ([start, rest] : xs) = execMapping (findMapping' start) rest : solve2 xs
solve2 _ = undefined

part1 :: String -> String
part1 = evaluate (concatMap (words . (!! 1) . wordsBy (== '|')) . lines) (length . filter unique . map length)

part2 :: String -> String
part2 = evaluate (map (map words . wordsBy (== '|')) . lines) (sum . solve2)

main :: IO ()
main = interact part2
