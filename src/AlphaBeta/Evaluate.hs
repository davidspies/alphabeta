module AlphaBeta.Evaluate where

class Evaluate f where
  wrap :: a -> f a
  evaluate :: Ord a => f a -> a
