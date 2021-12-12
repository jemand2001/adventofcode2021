module Types where

import Control.Applicative

data Pair a = P {first :: a, second :: a} deriving (Ord, Eq)

newtype Grid a = G [[a]] deriving Eq

instance Functor Pair where
  fmap f (P a b) = P (f a) (f b)

instance Applicative Pair where
  pure x = P x x
  P f g <*> P a b = P (f a) (g b)

instance Foldable Pair where
  foldr f d (P a b) = foldr f d [a, b]

instance (Show a) => Show (Pair a) where
  show (P a b) = show (a, b)

instance (Semigroup a) => Semigroup (Pair a) where
  P a1 b1 <> P a2 b2 = P (a1 <> a2) (b1 <> b2)

instance (Monoid a) => Monoid (Pair a) where
  mempty = P mempty mempty

instance (Num a) => Num (Pair a) where
  P a b + P c d = P (a + c) (b + d)
  P a b * P c d = P (a * c) (b * d)
  abs (P a b) = P (abs a) (abs b)
  signum (P a b) = P (signum a) (signum b)
  fromInteger x = P (fromInteger x) (fromInteger x)
  negate (P a b) = P (negate a) (negate b)

instance (Num a) => Num (Grid a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance Foldable Grid where
  foldr f d (G l) = foldr f d $ concat l

instance Functor Grid where
  fmap f (G l) = G $ map (map f) l

instance Applicative Grid where
  pure x = G [[x]]
  liftA2 f (G xss) (G yss) = G (zipWith (zipWith f) xss yss)

fromList :: [a] -> Pair a
fromList [a, b] = P a b
fromList _ = undefined

withDimensions :: a -> Grid b -> Grid a
withDimensions x (G xss) = G [[x | _ <- [1 .. length $ head xss]] | _ <- [1 .. length xss]]

set :: Pair Int -> a -> Grid a -> Grid a
set (P x y) v (G l) = G [[if x == x' && y == y' then v else v' | (x', v') <- zip [0 ..] row] | (y', row) <- zip [0 ..] l]

withCoordinates :: Grid a -> [(Pair Int, a)]
withCoordinates (G xss) = concat [[(P x y, v) | (x, v) <- zip [0 ..] row] | (y, row) <- zip [0 ..] xss]

coordinates :: Grid a -> [Pair Int]
coordinates = map fst . withCoordinates
