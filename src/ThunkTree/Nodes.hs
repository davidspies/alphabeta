module ThunkTree.Nodes
  ( MaxNode,
    MinNode,
    leaf,
    maxNode,
    minNode,
  )
where

import Convert (Convert (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Evaluate (Evaluate (..))
import LazyMax (LazyMax)
import qualified LazyMax
import LazyMin (LazyMin)
import qualified LazyMin

data MaxNode a = MaxLeaf a | MaxBranch (LazyMax (MinNode a))

data MinNode a = MinLeaf a | MinBranch (LazyMin (MaxNode a))

leaf :: a -> MaxNode a
leaf = MaxLeaf

maxNode :: NonEmpty (MinNode a) -> MaxNode a
maxNode = MaxBranch . LazyMax.maximum

minNode :: NonEmpty (MaxNode a) -> MinNode a
minNode = MinBranch . LazyMin.minimum

instance Evaluate MaxNode where
  evaluate = \case
    MaxLeaf x -> x
    MaxBranch x -> evaluate $ evaluate x

instance Evaluate MinNode where
  evaluate = \case
    MinLeaf x -> x
    MinBranch x -> evaluate $ evaluate x

instance Convert MinNode MaxNode where
  convert n = case n of
    MinLeaf x -> MaxLeaf x
    MinBranch _ -> MaxBranch $ LazyMax.maximum (n :| [])

instance Convert MaxNode MinNode where
  convert n = case n of
    MaxLeaf x -> MinLeaf x
    MaxBranch _ -> MinBranch $ LazyMin.minimum (n :| [])

instance Ord a => Eq (MaxNode a) where
  x == y = x <= y && y <= x

instance Ord a => Ord (MaxNode a) where
  MaxLeaf x <= MaxLeaf y = x <= y
  x <= y = unpackMaxBranch x <= unpackMaxBranch y

unpackMaxBranch :: MaxNode a -> LazyMax (MinNode a)
unpackMaxBranch = \case
  MaxLeaf x -> LazyMax.maximum (MinLeaf x :| [])
  MaxBranch x -> x

instance Ord a => Eq (MinNode a) where
  x == y = x <= y && y <= x

instance Ord a => Ord (MinNode a) where
  MinLeaf x <= MinLeaf y = x <= y
  x <= y = unpackMinBranch x <= unpackMinBranch y

unpackMinBranch :: MinNode a -> LazyMin (MaxNode a)
unpackMinBranch = \case
  MinLeaf x -> LazyMin.minimum (MaxLeaf x :| [])
  MinBranch x -> x
