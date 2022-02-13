{-# OPTIONS_GHC -Wno-orphans #-}

module Memoize.Orphans where

import Data.Function.Memoize (Memoize (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector (Vector)
import qualified Data.Vector as V

instance Memoize a => Memoize (Vector a) where
  memoize f = memoize (f . V.fromList) . V.toList

instance Memoize a => Memoize (NonEmpty a)
