module Main where

import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace
import Types
import Utils

data Cave = Small String | Big String | Start | End deriving (Eq, Ord)

instance Show Cave where
  show Start = "start"
  show End = "end"
  show (Small s) = toLower <$> s
  show (Big s) = toUpper <$> s

type Edge = Pair Cave

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end" = End
parseCave s
  | all isUpper s = Big $ map toLower s
  | all isLower s = Small s
  | otherwise = undefined

parseEdges :: String -> [Edge]
parseEdges = concatMap (getEdges . wordsBy (== '-')) . lines
  where
    getEdges [a, b]
      | caveA == Start || caveB == End = [P caveA caveB]
      | caveB == Start || caveA == End = [P caveB caveA]
      | otherwise = [P caveA caveB, P caveB caveA]
      where
        caveA = parseCave a
        caveB = parseCave b
    getEdges _ = undefined

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

findPaths :: [Edge] -> [[Cave]]
findPaths es = findPaths' [] Start
  where
    findPaths' current End = [current]
    findPaths' current start = concatMap (findPaths' (start : current)) [start' | P caveA start' <- es, caveA == start, not (start' `elem` current && isSmall start')]

findPaths2 :: ([Edge], [Cave]) -> [[Cave]]
findPaths2 (es, caves) = findPaths' [] Start
  where
    findPaths' path End
      | 1 < countBy ((1 <) . count path) (filter isSmall caves) = []  -- if there are multiple double small caves, discard the path
      | otherwise = [path]
    findPaths' current start = concatMap (findPaths' (start : current)) [start' | P caveA start' <- es, caveA == start, satisfiesReq start' current]
    satisfiesReq (Big _) _ = True
    -- for some reason this doesn't always catch double doubles (if you know why, please leave an issue or something)
    satisfiesReq c@(Small _) path = c `notElem` path || all (\c' -> count path c' < 2) (filter isSmall caves)
    satisfiesReq _ _ = True

part1 :: String -> String
part1 = evaluate parseEdges (length . findPaths)

part2 :: String -> String
part2 = evaluate (caves . parseEdges) (length . findPaths2)
  where
    caves es = (es, nub $ map first es `union` map second es)

main :: IO ()
main = interact part2
