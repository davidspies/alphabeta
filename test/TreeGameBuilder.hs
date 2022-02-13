{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module TreeGameBuilder where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import TreeGame (Position (..))
import ZeroSumGame (Player (MinPlayer), opposite)
import Prelude

class Alternating t a | a -> t where
  player :: proxy a -> Player
  build :: HasCallStack => a -> Position t

newtype L a = L a

instance Alternating a (L a) where
  player _ = MinPlayer
  build (L x) = Leaf x

instance Alternating t a => Alternating t [a] where
  player _ = opposite $ player (Proxy @a)
  build xs = Branch (player $ Proxy @[a]) (NonEmpty.fromList $ build <$> xs)
