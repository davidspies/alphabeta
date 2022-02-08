module Evaluate where

class Evaluate f where
  evaluate :: Ord a => f a -> a
