module Abs.Store where

import qualified Data.IntMap as IntMap

type Store v = IntMap.IntMap v

newtype Loc i = Loc { unLoc :: Int }
  deriving (Eq, Ord, Show)
