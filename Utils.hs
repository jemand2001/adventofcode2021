module Utils where

count :: Eq a => [a] -> a -> Int
count l x = length $ filter (== x) l
