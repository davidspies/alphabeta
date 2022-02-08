{-# LANGUAGE UndecidableInstances #-}

module ThunkTree (ThunkTree, leaf, evaluate) where

import Convert (Convert (..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Evaluate (Evaluate (..))
import Extrema (Extrema (..))
import System.IO.Unsafe (unsafePerformIO)
import ThunkTree.Descend (descend)
import ThunkTree.Inner (ThunkTreeInner, maxNode, minNode)
import qualified ThunkTree.Inner as Inner (leaf)

newtype ThunkTree a = ThunkTree (IORef (ThunkTreeInner a))

instance Extrema (ThunkTree a) where
  maximum = makeThunkTree . maxNode . fmap convert
  minimum = makeThunkTree . minNode . fmap convert

makeThunkTree :: ThunkTreeInner a -> ThunkTree a
makeThunkTree = unsafePerformIO . fmap ThunkTree . newIORef . descend

getInner :: ThunkTree a -> ThunkTreeInner a
getInner (ThunkTree x) = unsafePerformIO $ do
  modifyIORef x descend
  readIORef x

instance (Evaluate a, Convert ThunkTreeInner a) => Convert ThunkTree a where
  convert = convert . getInner

leaf :: a -> ThunkTree a
leaf = makeThunkTree . Inner.leaf

instance Evaluate ThunkTree where
  evaluate = evaluate . getInner
