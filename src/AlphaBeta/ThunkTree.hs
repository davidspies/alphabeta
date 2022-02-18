{-# LANGUAGE UndecidableInstances #-}

module AlphaBeta.ThunkTree (ThunkTree, evaluate, wrap) where

import AlphaBeta.Convert (Convert (..))
import AlphaBeta.Evaluate (Evaluate (..))
import AlphaBeta.ThunkTree.Nodes (MaxNode, MinNode)
import qualified AlphaBeta.ThunkTree.Nodes as Nodes
import Extrema (Extrema (..))

data ThunkTree a = ThunkTreeLeaf a | ThunkTreeMax (MaxNode a) | ThunkTreeMin (MinNode a)

instance Ord a => Extrema (ThunkTree a) where
  maximum = ThunkTreeMax . Nodes.maxNode . fmap convert
  minimum = ThunkTreeMin . Nodes.minNode . fmap convert

instance Evaluate ThunkTree where
  wrap = ThunkTreeLeaf
  evaluate = \case
    ThunkTreeLeaf x -> x
    ThunkTreeMax y -> evaluate y
    ThunkTreeMin y -> evaluate y

instance (Evaluate a, Convert MinNode a, Convert MaxNode a) => Convert ThunkTree a where
  convert = \case
    ThunkTreeLeaf x -> wrap x
    ThunkTreeMax n -> convert n
    ThunkTreeMin n -> convert n
