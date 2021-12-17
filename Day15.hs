module Main where

import Data.Maybe
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as M
import Utils
import Types hiding (inside)
import Data.Foldable
import Data.Ord
import Debug.Trace

type Scores = M.Map Point InfInt

data PrioWrapper a = PW InfInt a deriving Show

instance (Eq (PrioWrapper a)) where
  PW x _ == PW y _ = x == y

instance (Ord (PrioWrapper a)) where
  PW x _ <= PW y _ = x <= y

data InfInt = X Int | Infinity deriving (Eq, Show)

instance Num InfInt where
  X a + X b = X $ a + b
  Infinity + _ = Infinity
  _ + Infinity = Infinity
  X a * X b = X $ a * b
  Infinity * _ = Infinity
  _ * Infinity = Infinity
  abs (X x) = X (abs x)
  abs Infinity = Infinity
  negate (X x) = X (negate x)
  negate Infinity = undefined
  signum (X x) = X (signum x)
  signum Infinity = 1
  fromInteger = X . fromInteger

instance Ord InfInt where
  _ <= Infinity = True
  Infinity <= _ = False
  X a <= X b = a <= b

fromInf :: InfInt -> Int
fromInf (X x) = x
fromInf Infinity = undefined


inside :: Point -> Int -> Bool
inside (P x y) gridSize = x >= 0 && x < gridSize && y >= 0 && y < gridSize

neighbors :: Point -> Int -> [Point]
neighbors current size = filter (`inside` size) $ map (+ current) [north, south, east, west]

reconstruct :: M.Map Point Point -> Point -> [Point]
reconstruct prev target
  | target == P 0 0 = []
  | isJust p = target:reconstruct prev target'
  | otherwise = []
  where
    p = M.lookup target prev
    target' = fromJust p

dijkstra :: Point -> Point -> Grid Int -> [Point]
dijkstra start goal grid = dijkstra' (M.insert start 0 $ M.fromList $ zip points $ repeat Infinity) (Q.singleton 0 start) M.empty
  where
    size = first $ dimensions grid
    points = coordinates grid
    dijkstra' :: Scores -> Q.MinPQueue InfInt Point -> M.Map Point Point -> [Point]
    dijkstra' dist q prev
      | Q.null q || current == goal = reconstruct prev goal
      | otherwise = dijkstra' dist' q' prev'
      where
        ((_, current), q'') = Q.deleteFindMin q
        (dist', prev', q') = foldr replaceMaybe (dist, prev, q'') $ neighbors current size
        replaceMaybe v t@(d, p, q)
          | alt < fromMaybe Infinity (M.lookup v dist) = (M.insert v alt d, M.insert v current p, Q.insert alt v q)
          | otherwise = t
          where
            alt = fromJust (M.lookup current dist) + fromIntegral (grid `at` v)

part1 :: String -> String
part1 = evaluate parseGrid (\g -> sum $ map (at g) $ dijkstra (P 0 0) (fromIntegral (gridSize g -1)) g)

gridSize :: Grid a -> Int
gridSize g = first $ dimensions g

part2 :: String -> String
part2 = evaluate parseGrid' (\g -> sum $ map (at g) $ dijkstra (P 0 0) (fromIntegral (gridSize g -1)) g)
-- part2 = evaluate parseGrid' (\g -> dijkstra (P 0 0) (fromIntegral (gridSize g -1)) g)
-- part2 = evaluate parseGrid' id
  where
    parseGrid' s = r `appendY` (add 1 <$> r) `appendY` (add 2 <$> r) `appendY` (add 3 <$> r) `appendY` (add 4 <$> r)
      where
        g = parseGrid s
        r = g `appendX` (add 1 <$> g) `appendX` (add 2 <$> g) `appendX` (add 3 <$> g) `appendX` (add 4 <$> g)
        add x y = if x + y < 10 then x + y else x + y - 9

main :: IO ()
main = interact part2
