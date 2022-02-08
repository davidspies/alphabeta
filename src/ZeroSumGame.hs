module ZeroSumGame where

import Data.Function.Memoize (Memoize)
import Extrema (Extrema, maximum, minimum)
import GHC.Generics (Generic)
import Game (Game, GameStatus)
import qualified Game
import Prelude hiding (maximum, minimum)

newtype ZeroSumGame game = ZeroSumGame {getZeroSumGame :: game}

data Player = MaxPlayer | MinPlayer
  deriving (Eq, Ord, Generic, Memoize)

opposite :: Player -> Player
opposite = \case
  MaxPlayer -> MinPlayer
  MinPlayer -> MaxPlayer

class Extrema (Outcome game) => IsZeroSumGame game where
  type Move game
  type Position game
  type Outcome game
  status :: game -> Position game -> GameStatus (Outcome game) Player (Move game)
  makeMove :: game -> Position game -> Move game -> Position game

  --- Heuristic for ordering moves in alpha-beta search.
  --- Higher values take priority over lower ones.
  --- A good heuristic can greatly reduce the solve-time.
  heuristic :: game -> Position game -> Move game -> Int
  heuristic _ _ _ = 0

instance IsZeroSumGame game => Game (ZeroSumGame game) where
  type Move (ZeroSumGame game) = Move game
  type Position (ZeroSumGame game) = Position game
  type Player (ZeroSumGame game) = Player
  type Outcome (ZeroSumGame game) = Outcome game
  status = status . getZeroSumGame
  makeMove = makeMove . getZeroSumGame
  bestOutcome _ = \case
    MaxPlayer -> maximum
    MinPlayer -> minimum
