module Types where

data Pair a = P {first :: a, second :: a} deriving (Ord, Eq)

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

fromList :: [a] -> Pair a
fromList [a, b] = P a b
fromList _ = undefined
