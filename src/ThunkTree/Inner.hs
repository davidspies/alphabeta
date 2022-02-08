{-# LANGUAGE UndecidableInstances #-}

module ThunkTree.Inner
  ( ThunkTreeInner,
    descendStep,
    leaf,
    maxNode,
    minNode,
  )
where

import Convert (Convert (..))
import Data.List.NonEmpty (NonEmpty)
import Evaluate (Evaluate (..))
import ThunkTree.Nodes hiding (leaf, maxNode, minNode)
import qualified ThunkTree.Nodes as Nodes

data ThunkTreeInner a = ThunkTreeMax (MaxNode a) | ThunkTreeMin (MinNode a)

descendStep :: ThunkTreeInner a -> IO (Maybe (ThunkTreeInner a))
descendStep = \case
  ThunkTreeMax x -> fmap ThunkTreeMin <$> getResolvedMax x
  ThunkTreeMin x -> fmap ThunkTreeMax <$> getResolvedMin x

instance Evaluate ThunkTreeInner where
  evaluate = \case
    ThunkTreeMax y -> evaluate y
    ThunkTreeMin y -> evaluate y

instance (Evaluate a, Convert MinNode a, Convert MaxNode a) => Convert ThunkTreeInner a where
  convert = \case
    ThunkTreeMax n -> convert n
    ThunkTreeMin n -> convert n

leaf :: a -> ThunkTreeInner a
leaf = ThunkTreeMax . Nodes.leaf

maxNode :: NonEmpty (MinNode a) -> ThunkTreeInner a
maxNode = ThunkTreeMax . Nodes.maxNode

minNode :: NonEmpty (MaxNode a) -> ThunkTreeInner a
minNode = ThunkTreeMin . Nodes.minNode
