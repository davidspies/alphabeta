module MiniMax where

import Data.Function.Memoize (Memoize, memoize)
import Game (Game (..), GameStatus (..))

solve :: (Game game, Memoize (Position game)) => game -> Position game -> Outcome game
solve game = go
  where
    go = memoize $ \pos -> case status game pos of
      GameOver {outcome} -> outcome
      GameState {turn, moves} -> bestOutcome game turn (go . makeMove game pos <$> moves)
