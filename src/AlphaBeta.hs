{-# LANGUAGE UndecidableInstances #-}

module AlphaBeta (solve) where

import Data.Function.Memoize (Memoize)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (Down (Down))
import Game (Game, GameStatus (..))
import qualified MiniMax
import ThunkTree (ThunkTree, evaluate, leaf)
import ZeroSumGame (IsZeroSumGame (..), ZeroSumGame (..))
import Prelude

newtype AlphaBeta game = AlphaBeta {getAlphaBeta :: game}

deriving via
  ZeroSumGame (AlphaBeta game)
  instance
    (IsZeroSumGame game, Ord (Outcome game)) => Game (AlphaBeta game)

instance (IsZeroSumGame game, Ord (Outcome game)) => IsZeroSumGame (AlphaBeta game) where
  type Position (AlphaBeta game) = Position game
  type Move (AlphaBeta game) = Move game
  type Outcome (AlphaBeta game) = ThunkTree (Outcome game)
  status (AlphaBeta game) pos = case status game pos of
    GameOver {outcome} -> GameOver (leaf outcome)
    GameState {turn, moves} -> GameState {turn, moves = sortOn (Down . heuristic game pos) moves}
  makeMove = makeMove . getAlphaBeta
  heuristic = heuristic . getAlphaBeta

sortOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = NonEmpty.fromList . List.sortOn f . NonEmpty.toList

solve ::
  (IsZeroSumGame game, Memoize (Position game), Ord (Outcome game)) =>
  game ->
  Position game ->
  Outcome game
solve game = evaluate . MiniMax.solve (AlphaBeta game)
