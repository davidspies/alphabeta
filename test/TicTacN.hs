{-# LANGUAGE UndecidableInstances #-}

module TicTacN (TicTacN (..), Player (..), Position (..), WLTOutcome (..), start) where

import Control.Monad (guard)
import Data.Function.Memoize (Memoize)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, listToMaybe, maybeToList)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Extrema (Extrema)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Game (Game, GameStatus (GameOver, GameState, moves))
import qualified Game
import ZeroSumGame (IsZeroSumGame, Player (..), ZeroSumGame, opposite)
import qualified ZeroSumGame

data TicTacN = TicTacN {width :: Int, height :: Int, inARow :: Int}
  deriving (Game) via ZeroSumGame TicTacN

data Cell = X | O | E
  deriving (Eq, Ord, Generic, Memoize)

data Position = Position {turn :: Player, board :: Vector (Vector Cell)}
  deriving (Eq, Ord, Generic, Memoize)

data WLTOutcome = XLose | XDraw | XWin
  deriving (Eq, Ord, Show, Extrema)

playerToCell :: Player -> Cell
playerToCell = \case
  MaxPlayer -> X
  MinPlayer -> O

cellToPlayer :: Cell -> Maybe Player
cellToPlayer = \case
  X -> Just MaxPlayer
  O -> Just MinPlayer
  E -> Nothing

getWinner :: TicTacN -> Position -> Maybe Player
getWinner game Position {board} = listToMaybe $ do
  ((row, col) :| rcs) <- allWins game
  p0 <- maybeToList $ cellToPlayer $ board V.! row V.! col
  guard $ all (== playerToCell p0) [board V.! r V.! c | (r, c) <- rcs]
  return p0

allWins :: TicTacN -> [NonEmpty (Int, Int)]
allWins game@TicTacN {height, width, inARow} =
  let winsInDirection' = winsInDirection game
   in winsInDirection' [0 .. height - 1] [0 .. width - inARow] const (+)
        ++ winsInDirection' [0 .. height - inARow] [0 .. width - 1] (+) const
        ++ winsInDirection' [0 .. height - inARow] [0 .. width - inARow] (+) (+)
        ++ winsInDirection' [0 .. height - inARow] [inARow - 1 .. width - 1] (+) (-)

winsInDirection ::
  TicTacN ->
  [Int] ->
  [Int] ->
  (Int -> Int -> Int) ->
  (Int -> Int -> Int) ->
  [NonEmpty (Int, Int)]
winsInDirection TicTacN {inARow} rows cols modRow modCol = do
  row <- rows
  col <- cols
  return $ NonEmpty.fromList [(row `modRow` dx, col `modCol` dx) | dx <- [0 .. inARow - 1]]

getMoves :: TicTacN -> Position -> [(Int, Int)]
getMoves TicTacN {height, width} Position {board} =
  [ (row, col)
    | row <- [0 .. height - 1],
      col <- [0 .. width - 1],
      board V.! row V.! col == E
  ]

modify :: HasCallStack => Int -> (a -> a) -> Vector a -> Vector a
modify i f v = before V.++ V.cons (f h) t
  where
    (before, after) = V.splitAt i v
    (h, t) = fromJust $ V.uncons after

instance IsZeroSumGame TicTacN where
  type Position TicTacN = Position
  type Move TicTacN = (Int, Int)
  type Outcome TicTacN = WLTOutcome
  status game pos@Position {turn} = case getWinner game pos of
    Just MaxPlayer -> GameOver XWin
    Just MinPlayer -> GameOver XLose
    Nothing -> case nonEmpty (getMoves game pos) of
      Nothing -> GameOver XDraw
      Just moves -> GameState {Game.turn, moves}
  makeMove _ Position {board, turn} (row, col) =
    Position
      { board = modify row (modify col (const $ playerToCell turn)) board,
        turn = opposite turn
      }
  heuristic game _ =
    let winCounts :: Map (Int, Int) Int
        winCounts = M.fromListWith (+) [(rc, 1) | rcs <- allWins game, rc <- NonEmpty.toList rcs]
     in (winCounts M.!)

start :: TicTacN -> Position
start TicTacN {width, height} =
  Position {turn = MaxPlayer, board = V.replicate height $ V.replicate width E}
