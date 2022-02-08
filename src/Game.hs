module Game (Game (..), GameStatus (..)) where

import Data.List.NonEmpty (NonEmpty)
import GHC.Stack (HasCallStack)

class Game game where
  type Player game
  type Position game
  type Move game
  type Outcome game

  status :: game -> Position game -> GameStatus (Outcome game) (Player game) (Move game)
  makeMove :: HasCallStack => game -> Position game -> Move game -> Position game
  bestOutcome :: game -> Player game -> NonEmpty (Outcome game) -> Outcome game

data GameStatus outcome player move
  = GameOver {outcome :: outcome}
  | GameState {turn :: player, moves :: NonEmpty move}
