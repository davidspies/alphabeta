{-# LANGUAGE UndecidableInstances #-}

module ThunkTree (ThunkTree, evaluate, leaf) where

import Convert (Convert (..))
import Evaluate (Evaluate (..))
import Extrema (Extrema (..))
import ThunkTree.Nodes hiding (leaf, maxNode, minNode)
import qualified ThunkTree.Nodes as Nodes

data ThunkTree a = ThunkTreeMax (MaxNode a) | ThunkTreeMin (MinNode a)

instance Ord a => Extrema (ThunkTree a) where
  maximum = ThunkTreeMax . Nodes.maxNode . fmap convert
  minimum = ThunkTreeMin . Nodes.minNode . fmap convert

instance Evaluate ThunkTree where
  evaluate = \case
    ThunkTreeMax y -> evaluate y
    ThunkTreeMin y -> evaluate y

instance (Evaluate a, Convert MinNode a, Convert MaxNode a) => Convert ThunkTree a where
  convert = \case
    ThunkTreeMax n -> convert n
    ThunkTreeMin n -> convert n

leaf :: a -> ThunkTree a
leaf = ThunkTreeMax . Nodes.leaf
