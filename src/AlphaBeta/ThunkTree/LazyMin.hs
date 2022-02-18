module AlphaBeta.ThunkTree.LazyMin (LazyMin, minimum) where

import AlphaBeta.Evaluate (Evaluate (..))
import AlphaBeta.ThunkTree.LazyMax (LazyMax)
import qualified AlphaBeta.ThunkTree.LazyMax as LazyMax
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord (Down (..))
import Prelude hiding (maximum, minimum)

newtype LazyMin a = LazyMin (LazyMax (Down a))

minimum :: NonEmpty a -> LazyMin a
minimum xs = LazyMin $ LazyMax.maximum $ Down <$> xs

instance Evaluate LazyMin where
  wrap = minimum . (:| [])
  evaluate (LazyMin x) = getDown $ evaluate x

instance Ord a => Eq (LazyMin a) where
  x == y = x <= y && y <= x

instance Ord a => Ord (LazyMin a) where
  LazyMin x <= LazyMin y = y <= x
