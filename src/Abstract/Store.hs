{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeOperators #-}
module Abstract.Store where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Foldable (asum)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Semigroup

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


newtype Loc' a = Loc' { unLoc' :: String }
  deriving (Eq, Ord, Show)

type Store' a = Map.Map (Loc' a) [a]

find' :: forall a fs. (Alternative (Eff fs), State (Store' a) :< fs) => Loc' a -> Eff fs a
find' loc = do
  store <- get
  asum (return <$> ((store :: Store' a) Map.! loc))

alloc' :: Alternative m => String -> m (Loc' a)
alloc' x = pure (Loc' x)

ext' :: (State (Store' a) :< fs) => Loc' a -> a -> Eff fs ()
ext' loc val = modify (Map.insertWith (<>) loc [val])
