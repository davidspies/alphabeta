{-# LANGUAGE UndecidableInstances #-}

module AlphaBeta.ThunkTree (ThunkTree, evaluate, leaf) where

import AlphaBeta.Convert (Convert (..))
import AlphaBeta.Evaluate (Evaluate (..))
import AlphaBeta.ThunkTree.Nodes (MaxNode, MinNode)
import qualified AlphaBeta.ThunkTree.Nodes as Nodes
import Extrema (Extrema (..))

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
