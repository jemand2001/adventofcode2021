module Main where

import Data.List
import Data.List.Split
import Types
import Utils

data Instruction = Vertical Int | Horizontal Int

parseInput :: String -> ([Pair Int], [Instruction])
parseInput = (\[a, b] -> (map toPair a, map parseInstruction b)) . wordsBy null . lines
  where
    toPair = (\[x, y] -> P (read x) (read y)) . wordsBy (== ',')

parseInstruction :: String -> Instruction
parseInstruction s
  | firstHalf == "fold along x" = Vertical $ read secondHalf
  | firstHalf == "fold along y" = Horizontal $ read secondHalf
  | otherwise = undefined
  where
    [firstHalf, secondHalf] = wordsBy (== '=') s

fold :: Int -> Int -> Int
fold y y'
  | y < y' = y
  | y == y' = undefined
  | otherwise = y' - (y - y')

executeFold :: Instruction -> [Pair Int] -> [Pair Int]
executeFold (Horizontal y') dots = [P x (fold y y') | (P x y) <- dots]
executeFold (Vertical x') dots = [P (fold x x') y | (P x y) <- dots]

executeFolds :: [Pair Int] -> [Instruction] -> [Pair Int]
executeFolds = foldl (flip executeFold)

showDots :: [Pair Int] -> String
showDots dots = intercalate "\n" [[if P x y `elem` dots then '#' else ' ' | x <- [0..maxX+1]] | y <- [0..maxY+1]] ++ "\n"
  where
    maxX = maximum $ map first dots
    maxY = maximum $ map second dots

part1 :: String -> String
part1 = evaluate parseInput (length . nub . (\(dots, i : _) -> executeFold i dots))

part2 :: String -> String
part2 = evaluate' parseInput (map abs . nub . uncurry executeFolds) showDots

main :: IO ()
main = interact part2
