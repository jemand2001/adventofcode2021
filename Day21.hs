{-# LANGUAGE TupleSections #-}

module Main where

import Types
import Utils (evaluate)
import Debug.Trace (trace, traceShowId)

data Player = Player {score :: Integer, position :: Integer} deriving (Show, Eq)

add1 :: Integer -> Player -> (Integer, Player)
add1 d (Player s p) = (d', player)
  where
    d'
      | d > 100 = trace "going back to 1" 0
      | otherwise = d + 1
    p' = ((p + d') `mod` 10) + 1
    player = Player (s + p') p'

game :: (Integer, Integer, Pair Player) -> Integer
game (last, num, P p1 p2@Player {score = s2})
  | s1' >= 1000 = s2 * num'
  | s2' >= 1000 = s1' * num''
  | otherwise = game (next', num'', P p1' p2')
  where
    (next, p1'@Player {score = s1'}) = foldr (const $ uncurry add1) (last, p1) $ traceShowId [0 .. 2]
    num' = num + 3
    (next', p2'@Player {score = s2'}) = foldr (const $ uncurry add1) (next, p2) [0 .. 2]
    num'' = num' + 3

part1 :: String -> String
part1 = evaluate ((0,0,) . fromList . map (Player 0 . read . reverse . head . words . reverse) . lines) game

main :: IO ()
main = interact part1
