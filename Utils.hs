module Utils where
import Data.List.Split (wordsBy)

count :: Eq a => [a] -> a -> Int
count l x = length $ filter (== x) l

evaluate :: (Show a) => (String -> b) -> (b -> a) -> String -> String
evaluate parse f = evaluate' parse f ((++ "\n") . show)

evaluate' :: (String -> b) -> (b -> a) -> (a -> String) -> String -> String
evaluate' parse f toString = toString . f . parse

readXs :: (Read a) => String -> [a]
readXs = map read . wordsBy (== ',')
