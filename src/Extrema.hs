module Extrema where

import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (maximum, minimum)
import qualified Prelude as P

class Extrema a where
  maximum :: NonEmpty a -> a
  minimum :: NonEmpty a -> a

newtype OptOrd a = OptOrd a
  deriving (Eq, Ord)

instance Ord a => Extrema (OptOrd a) where
  maximum = P.maximum
  minimum = P.minimum

deriving via OptOrd Int instance Extrema Int
