module Abs.Store where

newtype Loc i = Loc { unLoc :: Int }
  deriving (Eq, Ord, Show)
