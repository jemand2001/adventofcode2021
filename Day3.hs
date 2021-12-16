module Main where

import Data.Foldable
import Data.List
import Data.Ord

import Types (Pair(..))
import Utils (count, evaluate, binToInt)

minMaxCounts :: Eq a => [a] -> Pair (Int, a)
minMaxCounts l = P (minimumBy (comparing fst) l') (maximumBy (comparing fst) l')
  where l' = map (\x -> (count l x, x)) l

minMax :: Eq a => [a] -> Pair a
minMax = (snd <$>) . minMaxCounts

gammaEpsilon :: [String] -> Pair String
gammaEpsilon = foldr (\ l -> (<*>) ((:) <$> minMax l)) (pure [])

oxygen :: [String] -> String
oxygen [l] = l
oxygen ls = mc:oxygen (map tail $ filter ((== mc) . head) ls)
  where
    (P (cl, l) (cm, m)) = minMaxCounts (map head ls)
    mc = if cl == cm then '1' else m

co2 :: [String] -> String
co2 [l] = l
co2 ls = lc: co2 (map tail $ filter ((== lc) . head) ls)
  where
    (P (cl, l) (cm, m)) = minMaxCounts (map head ls)
    lc = if cl == cm then '0' else l

part1 :: String -> String
part1 = evaluate (transpose . lines) (product . (binToInt <$>) . gammaEpsilon)

part2 :: String -> String
part2 = evaluate lines (product . (binToInt <$>) . (\ls -> P (oxygen ls) (co2 ls)))

main :: IO ()
main = interact part2
