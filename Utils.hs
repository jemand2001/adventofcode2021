module Utils where
import Data.List.Split (wordsBy)

import Types
import Debug.Trace

count :: (Eq a, Foldable t) => t a -> a -> Int
count xs x = countBy (== x) xs

countBy :: Foldable t => (a -> Bool) -> t a -> Int
countBy p = foldr (\x c -> if p x then c+1 else c) 0

countTrue :: Foldable t => t Bool -> Int
countTrue = countBy id

evaluate :: (Show a) => (String -> b) -> (b -> a) -> String -> String
evaluate parse f = evaluate' parse f ((++ "\n") . show)

evaluate' :: (String -> b) -> (b -> a) -> (a -> String) -> String -> String
evaluate' parse f toString = toString . f . parse

readXs :: (Read a) => String -> [a]
readXs = map read . wordsBy (== ',')

lookupBy :: (a -> Bool) -> [a] -> Maybe a
lookupBy _ [] = Nothing
lookupBy f (x:xs)
  | f x = Just x
  | otherwise = lookupBy f xs

parseGrid :: String -> Grid Int
parseGrid = G . map (map (read . (: []))) . lines

at :: Grid a -> Pair Int -> a
at (G xss) (P x y) = xss !! y !! x

north, south, east, west, northwest, northeast, southwest, southeast :: Pair Int
[northwest, north, northeast, west, _, east, southwest, south, southeast] = [P x y | x <- [-1..1], y <- [-1..1]]

directions :: [Pair Int]
directions = [northwest, north, northeast, west, east, southwest, south, southeast]

findAll :: (a -> Bool) -> Grid a -> [Pair Int]
findAll p = map fst . filter (p . snd) . withCoordinates

dimensions :: Grid a -> Pair Int
dimensions (G l) = P (length $ head l) $ length l

countsBy :: (a -> a -> Bool) -> [a] -> [Int]
countsBy f xs = map (\x -> countBy (f x) xs) xs

counts :: Eq a => [a] -> [Int]
counts = countsBy (==)

minMax :: Ord a => [a] -> Pair a
minMax xs = P (minimum xs) (maximum xs)

-- modified to work with strings, from https://stackoverflow.com/a/48438340/13321308
binToInt :: String -> Int
binToInt = foldr (\x y -> fromChar x + 2 * y) 0 . reverse
  where
    fromChar '0' = 0
    fromChar '1' = 1
    fromChar _ = undefined

tsnoc :: a -> b -> b
tsnoc _ x = x
