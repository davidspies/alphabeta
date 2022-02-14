module LazyMax (LazyMax, maximum) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Evaluate (Evaluate (..))
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (maximum)

newtype LazyMax a = LazyMax {getLazyMax :: IORef (a, [a])}

step :: Ord a => LazyMax a -> IO ()
step (LazyMax ref) = do
  (x, xs) <- readIORef ref
  case xs of
    [] -> return ()
    (y : ys) -> writeIORef ref (max x y, ys)

instance Evaluate LazyMax where
  evaluate a@(LazyMax ref) = unsafePerformIO go
    where
      go = do
        (x, xs) <- readIORef ref
        if null xs then return x else step a >> go

maximum :: NonEmpty a -> LazyMax a
maximum (x :| xs) = LazyMax $ unsafePerformIO $ newIORef (x, xs)

instance Ord a => Eq (LazyMax a) where
  x == y = x <= y && y <= x

instance Ord a => Ord (LazyMax a) where
  x <= y = unsafePerformIO go
    where
      go = do
        (xCur, xRest) <- readIORef $ getLazyMax x
        (yCur, yRest) <- readIORef $ getLazyMax y
        if xCur <= yCur
          then if null xRest then return True else step x >> go
          else if null yRest then return False else step y >> go
