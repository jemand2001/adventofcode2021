module Main where

import Data.Default
import Data.Foldable

data Sub = Sub {depth :: Int, distance :: Int}
instance Default Sub where
  def = Sub 0 0

data Sub2 = Sub2 {depth2 :: Int, distance2 :: Int, aim :: Int}
instance Default Sub2 where
  def = Sub2 0 0 0

data Command = Forward Int | Up Int | Down Int

process :: Sub -> [Command] -> Sub
process s = foldl execute s
  where
    execute s@Sub {depth = d}    (Up y)       = s {depth = d - y}
    execute s@Sub {depth = d}    (Down y)     = s {depth = d + y}
    execute s@Sub {distance = d} (Forward x)  = s {distance = d + x}

process2 :: Sub2 -> [Command] -> Sub2
process2 s = foldl execute s
  where
    execute s@Sub2 {aim=a}   (Up t)      = s {aim = a - t}
    execute s@Sub2 {aim=a}   (Down t)    = s {aim = a + t}
    execute s@(Sub2 de di a) (Forward x) = s {distance2 = di + x, depth2 = de + (x * a)}

part1 :: String -> String
part1 = (++ "\n") . show . (\(Sub x y) -> x * y) . process def . map (parseCmd . words) . lines

part2 :: String -> String
part2 = (++ "\n") . show . (\(Sub2 x y _) -> x * y) . process2 def . map (parseCmd . words) . lines

parseCmd :: [String] -> Command
parseCmd ["forward", x] = Forward $ read x
parseCmd ["up", x] = Up $ read x
parseCmd ["down", x] = Down $ read x
parseCmd _ = undefined

main :: IO ()
main = interact part2
