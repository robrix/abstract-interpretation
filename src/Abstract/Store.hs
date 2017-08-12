{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeOperators #-}
module Abstract.Store where

import Control.Monad.Effect
import Control.Monad.Effect.State
import qualified Data.IntMap as IntMap

type Store v = IntMap.IntMap v

newtype Loc i = Loc { unLoc :: Int }
  deriving (Eq, Ord, Show)

find :: (State (Store a) :< fs) => Loc a -> Eff fs a
find = flip fmap get . flip (IntMap.!) . unLoc

alloc :: forall a fs . (State (Store a) :< fs) => String -> Eff fs (Loc a)
alloc _ = do
  s <- get
  return (Loc (length (s :: Store a)))

ext :: (State (Store a) :< fs) => Loc a -> a -> Eff fs ()
ext (Loc loc) val = modify (IntMap.insert loc val)
