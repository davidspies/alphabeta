{-# LANGUAGE TypeApplications #-}

module LibTest where

import qualified AlphaBeta
import Control.Exception (Exception, evaluate)
import qualified MiniMax
import Test.Hspec
import TicTacN (TicTacN (..), WLTOutcome (XDraw), start)
import TreeGame (TreeGame (TreeGame))
import TreeGameBuilder (L (L), build)

spec_tictactoe :: Spec
spec_tictactoe =
  describe "TicTacToe" $
    it "should be a draw" $
      do
        let game = TicTacN {height = 3, width = 3, inARow = 3}
        AlphaBeta.solve game (start game)
          `shouldBe` XDraw

spec_treegame :: Spec
spec_treegame =
  describe "TreeGame" $ do
    it "should solve a simple game" $
      AlphaBeta.solve (TreeGame id) (build @Int [[L 4, L 3], [L 5, L 6]])
        `shouldBe` 4
    let partialScoreFn :: Int -> Int
        partialScoreFn x = if x == 6 then error "scoring" else x
    it "should not evaluate things outside the proof tree" $ do
      MiniMax.solve (TreeGame partialScoreFn) (build @Int [[L 4, L 3], [L 5, L 6]])
        `evaluationThrows` errorCall "scoring"
      AlphaBeta.solve (TreeGame partialScoreFn) (build @Int [[L 4, L 3], [L 5, L 6]])
        `shouldBe` 4
      MiniMax.solve (TreeGame partialScoreFn) (build @Int [[[L 4], [L 5]], [[L 2], [L 6]]])
        `evaluationThrows` errorCall "scoring"
      AlphaBeta.solve (TreeGame partialScoreFn) (build @Int [[[L 4], [L 5]], [[L 2], [L 6]]])
        `shouldBe` 4

evaluationThrows :: (HasCallStack, Exception e) => a -> Selector e -> IO ()
evaluationThrows x selector = evaluate x `shouldThrow` selector
