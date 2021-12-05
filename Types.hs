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

fromList :: [a] -> Pair a
fromList [a, b] = P a b
fromList _ = undefined
