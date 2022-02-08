module LazyMin (LazyMin, minimum, getResolved) where

import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Down (..))
import Evaluate (Evaluate (..))
import LazyMax (LazyMax)
import qualified LazyMax
import Prelude hiding (maximum, minimum)

newtype LazyMin a = LazyMin (LazyMax (Down a))

minimum :: NonEmpty a -> LazyMin a
minimum xs = LazyMin $ LazyMax.maximum $ Down <$> xs

getResolved :: LazyMin a -> IO (Maybe a)
getResolved (LazyMin x) = fmap getDown <$> LazyMax.getResolved x

instance Evaluate LazyMin where
  evaluate (LazyMin x) = getDown $ evaluate x

instance Ord a => Eq (LazyMin a) where
  x == y = x <= y && y <= x

instance Ord a => Ord (LazyMin a) where
  LazyMin x <= LazyMin y = y <= x
