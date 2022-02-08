module Convert where

import Evaluate (Evaluate)

class (Evaluate a, Evaluate b) => Convert a b where
  convert :: a x -> b x

instance Evaluate a => Convert a a where
  convert = id
