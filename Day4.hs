{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable (maximumBy, minimumBy)
import Data.List
import Data.List.Split (wordsBy)
import Data.Ord (comparing)
import Debug.Trace
import Text.Read (readMaybe)

type Board = [[Maybe String]]

mark :: String -> Board -> Board
mark x = map (map $ \a -> if a == Just x then Nothing else a)

hasWon :: Board -> Bool
hasWon b = replicate 5 Nothing `elem` b || replicate 5 Nothing `elem` transpose b

bingo :: [String] -> [Board] -> (Board, Int)
bingo [] _ = undefined
bingo (x : xs) boards
  | any hasWon boards' = (,read x) $ head $ filter hasWon boards'
  | otherwise = bingo xs boards'
  where
    boards' = map (mark x) boards

bingo2 :: [String] -> [Board] -> (Board, Int)
bingo2 [] _ = undefined
bingo2 (x:xs) [b]
  | hasWon b' = (b', read x)
  | otherwise = bingo2 xs [b']
  where
    b' = mark x b
bingo2 (x : xs) boards
  | length notWon == 1 = bingo2 xs [mark x (head notWon)]
  | otherwise = bingo2 xs boards'
  where
    boards' = map (mark x) boards
    notWon = filter (not . hasWon) boards

score :: Board -> Int
score l = sum $ map (sum . map (maybe 0 read)) l

parseBoards :: [String] -> ([String], [Board])
parseBoards (l : "" : ls) = (wordsBy (',' ==) l, parseBoards' ls)
  where
    parseBoards' [] = []
    parseBoards' ls = map (map Just . words) (take 5 ls) : parseBoards' (drop 6 ls)
parseBoards _ = undefined

part1 :: String -> String
part1 = (++ "\n") . show . (\(b, x) -> x * score b) . uncurry bingo . parseBoards . lines

part2 :: String -> String
part2 = (++ "\n") . show . (\(b, x) -> x * score b) . uncurry bingo2 . parseBoards . lines

main :: IO ()
main = interact part2
