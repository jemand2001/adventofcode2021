module Main where
import Utils
import Types
import Data.List
import qualified Data.Map as M
import Data.Maybe

type Count = Integer
type RuleMap = M.Map (Pair Char) Char
type PairCounts = M.Map (Pair Char) Count
type CharCounts = M.Map Char Count

parseRule :: String -> (Pair Char, Char)
parseRule [c1, c2, ' ', '-', '>', ' ', c3] = (P c1 c2,  c3)
parseRule _ = undefined

countPairs :: String -> PairCounts
countPairs "" = M.empty
countPairs [c] = M.empty
countPairs (a:b:s) = M.insertWith (+)  (P a b) 1 $ countPairs $ b:s

addKeys :: Ord k => [k] -> M.Map k Count -> M.Map k Count
addKeys ks = addKeys' ks 1

addKeys' :: Ord k => [k] -> Count -> M.Map k Count -> M.Map k Count
addKeys' ks v m = foldr (`addKey'` v) m ks

addKey :: Ord k => k -> M.Map k Count -> M.Map k Count
addKey k = addKey' k 1

addKey' :: Ord k => k -> Count -> M.Map k Count -> M.Map k Count
addKey' = M.insertWith (+)

minMaxCounts :: Char -> Char -> PairCounts -> Pair Count
minMaxCounts first last = minMax . M.elems . addKeys [first, last] . M.foldrWithKey (\(P a b) v -> addKeys' [a, b] v) M.empty

pairInsert :: RuleMap -> PairCounts -> PairCounts
pairInsert rules = M.foldrWithKey insertPair M.empty
  where
    insertPair :: Pair Char -> Count -> PairCounts -> PairCounts
    insertPair p@(P a b) c = addKeys' [P a middle, P middle b] c
      where
        middle = fromJust $ M.lookup p rules

runSimulation :: Int -> String -> String
runSimulation count = evaluate parse (\(f, l, counts, rules) -> getDiff $ minMaxCounts f l $ foldr (const $ pairInsert rules) counts [1..count])
  where
    parse = (\(x : [] : rules) -> (head x, last x, countPairs x, M.fromList $ map parseRule rules)) . lines
    getDiff (P a b) = (b `div` 2) - (a `div` 2)

diagnostics :: Int -> String -> String
diagnostics count = evaluate parse (\(f, l, counts, rules) -> minMaxCounts f l $ foldr (const $ pairInsert rules) counts [1 .. count])
  where
    parse = (\(x : [] : rules) -> (head x, last x, countPairs x, M.fromList $ map parseRule rules)) . lines
    getDiff (P a b) = (b `div` 2) - (a `div` 2)

part1 :: String -> String
part1 = runSimulation 10

part2 :: String -> String
part2 = runSimulation 40

main :: IO ()
main = interact $ diagnostics 40
