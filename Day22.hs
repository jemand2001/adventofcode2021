module Main where

import Data.List (intersect)
import Data.List.Split (wordsBy)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Debug.Trace
import Utils (evaluate)

data Point = P {x :: Int, y :: Int, z :: Int} deriving (Eq, Ord)

type Core = Set Point

data Instruction = On {xs :: [Int], ys :: [Int], zs :: [Int]} | Off {xs :: [Int], ys :: [Int], zs :: [Int]}

parseInstruction :: String -> Instruction
parseInstruction s = cons xRange yRange zRange
  where
    [[cons'], ranges] = map (wordsBy (== ',')) $ words s
    cons
      | cons' == "on" = On
      | cons' == "off" = Off
      | otherwise = undefined
    [xRange, yRange, zRange] = map parseRange ranges
    parseRange (_ : '=' : rest) = [read start .. read fin]
      where
        [start, fin] = wordsBy (== '.') rest
    parseRange _ = undefined

execute :: Core -> [Instruction] -> Core
execute = foldl execute'
  where
    execute' c (On xs ys zs) = c `S.union` getPoints xs ys zs
    execute' c (Off xs ys zs) = c \\ getPoints xs ys zs

getPoints :: [Int] -> [Int] -> [Int] -> Set Point
getPoints xs ys zs = S.fromList [P x y z | x <- xs, y <- ys, z <- zs]

select :: [Int] -> [Int] -> [Int] -> Core -> Core
select xs ys zs = S.intersection $ getPoints xs ys zs

limit :: [Int] -> [Int] -> [Int] -> Instruction -> Instruction
limit xs ys zs (On xs' ys' zs') = On (xs `intersect` xs') (ys `intersect` ys') (zs `intersect` zs')
limit xs ys zs (Off xs' ys' zs') = Off (xs `intersect` xs') (ys `intersect` ys') (zs `intersect` zs')

part1 :: String -> String
part1 = evaluate (map parseInstruction . lines) (length . execute S.empty . map (limit [-50 .. 50] [-50 .. 50] [-50 .. 50]))

main :: IO ()
main = interact part1
