{-# LANGUAGE UndecidableInstances #-}

module TreeGame (TreeGame (..), Position (..)) where

import Data.Function.Memoize (Memoize)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Extrema (Extrema)
import GHC.Generics (Generic)
import Game (Game, GameStatus (..))
import MemoTrie.Orphans ()
import ZeroSumGame (IsZeroSumGame, Player, ZeroSumGame (..))
import qualified ZeroSumGame
import Prelude

newtype TreeGame a o = TreeGame (a -> o)
  deriving (Game) via ZeroSumGame (TreeGame a o)

data Position a = Leaf a | Branch Player (NonEmpty (Position a))
  deriving (Generic, Memoize)

instance Extrema o => IsZeroSumGame (TreeGame a o) where
  type Position (TreeGame a o) = Position a
  type Move (TreeGame a o) = Int
  type Outcome (TreeGame a o) = o
  status (TreeGame score) = \case
    Leaf x -> GameOver $ score x
    Branch turn children -> GameState {turn, moves = indices children}
  makeMove _ = \case
    Leaf _ -> error "Can't move from end-state"
    Branch _ ps -> (ps NonEmpty.!!)

indices :: NonEmpty a -> NonEmpty Int
indices = NonEmpty.zipWith const (0 :| [1 ..])
