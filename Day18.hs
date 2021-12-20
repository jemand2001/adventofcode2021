module Main where

import Data.Char (isDigit)
import Data.Foldable
import Data.List.Split (wordsBy)
import Data.Maybe (fromJust, isJust)
import Debug.Trace
import Text.Blaze.Html5 (rp)
import Types
import Utils

data Direction = L | R deriving (Show, Eq)

-- type Path = [Direction]
type Depth = Int

data Snailfish = N Int | SP Snailfish Snailfish deriving (Eq)

instance Show Snailfish where
  show (N x) = show x
  show (SP a b) = "[" ++ show a ++ "," ++ show b ++ "]"

type SnailfishChain = [(Direction, Depth, Int)]

inOrder :: Snailfish -> SnailfishChain
inOrder = inOrder' 1
  where
    inOrder' d (SP (N a) (N b)) = [(L, d, a), (R, d, b)]
    inOrder' d (SP (N a) r@(SP _ _)) = (L, d, a) : inOrder' (d + 1) r
    inOrder' d (SP l@(SP _ _) (N b)) = inOrder' (d + 1) l ++ [(R, d, b)]
    inOrder' d (SP l r) = inOrder' (d + 1) l ++ inOrder' (d + 1) r
    inOrder' _ _ = undefined

unfold :: SnailfishChain -> Snailfish
unfold [] = undefined
unfold [(R, _, x)] = N x
unfold ((L, d1, a) : (R, d2, b) : xs)
  | d1 == d2 && d1 > 1 = SP (SP (N a) (N b)) $ unfold xs
unfold ((L, d1, a) : xs@((L, d2, b) : _))
  | d1 < d2 = SP (N a) $ unfold xs
unfold a = error $ show a

plus :: SnailfishChain -> SnailfishChain -> SnailfishChain
plus a b = map (\(lr, d, x) -> (lr, d + 1, x)) $ a ++ b

-- instance Show Snailfish where
--   show (N x) = show x
--   show (SP a b) = '[':show a ++ "," ++ show b ++ "]"

-- zero :: Snailfish
-- zero = N 0

parseSnailfish :: String -> Snailfish
parseSnailfish = fst . parse' 0
  where
    parse' x ('[' : s)
      | '[' `notElem` untilClosing = (SP (N $ read a) (N $ read b), rest)
      | otherwise = (SP parsedFirst parsedSecond, s'')
      where
        (untilClosing, ']' : rest) = span (/= ']') s
        [a, b] = wordsBy (== ',') untilClosing
        (parsedFirst, ',' : s') = dropWhile (== ']') <$> parse' (x + 1) s
        (parsedSecond, s'') = parse' (x + 1) s'
    parse' x s = (N $ read number, rest)
      where
        s' = dropWhile (== ' ') s
        (number, rest) = span isDigit s'

findExplosion :: SnailfishChain -> Maybe Int
findExplosion [] = Nothing
findExplosion [_] = Nothing
findExplosion ((L, dl, a) : (R, dr, b) : xs)
  | dl == dr && dl >= 5 = trace (show (a, b)) $ Just 0
findExplosion (_ : xs) = (1 +) <$> findExplosion xs

explode :: SnailfishChain -> Int -> SnailfishChain
explode [(L, dx, x), (L, dl, l), (R, dr, r)] 1
  | dl == dr = [(L, dx, x + l), (R, dx, 0)]
explode ((L, dl, l) : (R, dr, r) : (R, dx, x) : xs) 0
  | dl == dr = (L, dx, 0) : (R, dx, r + x) : xs
explode ((L, dxl, xl) : (L, dl, l) : (R, dr, r) : (L, dxr, xr) : xs) 1
  | dl == dr = (L, dxl, xl + l) : (R, dxl, 0) : (L, dxr, xr + r) : xs
explode ((R, dxl, xl) : (L, dl, l) : (R, dr, r) : (R, dxr, xr) : xs) 1
  | dl == dr = (R, dxl, xl + l) : (L, dxr, 0) : (R, dxr, xr + r) : xs
explode (n : xs) x = n : explode xs (x - 1)
explode [] _ = undefined

findSplitting :: SnailfishChain -> Maybe Int
findSplitting [] = Nothing
findSplitting ((_, _, x) : xs)
  | x >= 10 = trace (show x) $ Just 0
  | otherwise = (1 +) <$> findSplitting xs

split :: SnailfishChain -> Int -> SnailfishChain
split spc x = start ++ [(L, d + 1, l), (R, d + 1, r)] ++ rest
  where
    start = take x spc
    (_, d, a) : rest = drop x spc
    l = a `div` 2
    r = if even a then a `div` 2 else (a + 1) `div` 2

eval :: SnailfishChain -> SnailfishChain
eval spc
  | isJust explosion = eval $ traceShowId $ explode spc (trace "explosion" fromJust explosion)
  | isJust splitting = eval $ traceShowId $ split spc (trace "split" fromJust splitting)
  | otherwise = spc
  where
    explosion = findExplosion spc
    splitting = findSplitting spc

magnitude :: Snailfish -> Int
magnitude (N x) = x
magnitude (SP a b) = 3 * magnitude a + 2 * magnitude b

part1 :: String -> String
-- part1 = evaluate (map parseSnailfish . lines) (magnitude . foldl1 (\a b -> eval $ SP a b))
part1 = evaluate (map (inOrder . parseSnailfish) . lines) (unfold . foldl1 (\a b -> eval $ a `plus` b))

--  (foldl1 (\a b -> eval $ a `plus` b))

main :: IO ()
main = interact part1
