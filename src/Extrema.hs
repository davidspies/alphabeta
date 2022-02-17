{-# LANGUAGE DefaultSignatures #-}

module Extrema where

import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (maximum, minimum)
import qualified Prelude as P

class Extrema a where
  maximum :: NonEmpty a -> a
  default maximum :: Ord a => NonEmpty a -> a
  maximum = P.maximum
  minimum :: NonEmpty a -> a
  default minimum :: Ord a => NonEmpty a -> a
  minimum = P.minimum

instance Extrema Int
