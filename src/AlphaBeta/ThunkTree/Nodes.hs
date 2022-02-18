module AlphaBeta.ThunkTree.Nodes (MaxNode, MinNode, maxNode, minNode) where

import AlphaBeta.Convert (Convert (..))
import AlphaBeta.Evaluate (Evaluate (..))
import AlphaBeta.ThunkTree.LazyMax (LazyMax)
import qualified AlphaBeta.ThunkTree.LazyMax as LazyMax
import AlphaBeta.ThunkTree.LazyMin (LazyMin)
import qualified AlphaBeta.ThunkTree.LazyMin as LazyMin
import Data.List.NonEmpty (NonEmpty ((:|)))

data MaxNode a = MaxLeaf a | MaxBranch (LazyMax (MinNode a))

data MinNode a = MinLeaf a | MinBranch (LazyMin (MaxNode a))

maxNode :: NonEmpty (MinNode a) -> MaxNode a
maxNode = MaxBranch . LazyMax.maximum

minNode :: NonEmpty (MaxNode a) -> MinNode a
minNode = MinBranch . LazyMin.minimum

instance Evaluate MaxNode where
  wrap = MaxLeaf
  evaluate = \case
    MaxLeaf x -> x
    MaxBranch x -> evaluate $ evaluate x

instance Evaluate MinNode where
  wrap = MinLeaf
  evaluate = \case
    MinLeaf x -> x
    MinBranch x -> evaluate $ evaluate x

instance Convert MinNode MaxNode where
  convert = MaxBranch . wrap

instance Convert MaxNode MinNode where
  convert = MinBranch . wrap

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
