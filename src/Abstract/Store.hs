{-# LANGUAGE FlexibleContexts, InstanceSigs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Abstract.Store where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Foldable (asum)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Semigroup

-- type Store f a = Map.Map (Loc a) (f a)

newtype LocI a = LocI { unLocI :: Int }
  deriving (Eq, Ord, Show)

newtype Loc a = Loc { unLoc :: String }
  deriving (Eq, Ord, Show)

class AbstractStore l where
  type Store l a

  find :: (State (Store l a) :< fs) => l a -> Eff fs a

  alloc :: (State (Store l a) :< fs) => String -> Eff fs (l a)

  ext :: (State (Store l a) :< fs) => l a -> a -> Eff fs ()

instance AbstractStore LocI where
  type Store LocI a = IntMap.IntMap a
  find = flip fmap get . flip (IntMap.!) . unLocI

  alloc :: forall a fs. (State (Store LocI a) :< fs) => String -> Eff fs (LocI a)
  alloc _ = do
    s <- get
    return (LocI (length (s :: Store LocI a)))

  ext (LocI loc) val = modify (IntMap.insert loc val)

find' :: forall a fs. (Alternative (Eff fs), State (Map.Map (Loc a) [a]) :< fs) => Loc a -> Eff fs a
find' loc = do
  store <- get
  asum (return <$> ((store :: Map.Map (Loc a) [a]) Map.! loc))

alloc' :: Alternative m => String -> m (Loc a)
alloc' x = pure (Loc x)

ext' :: (State (Map.Map (Loc a) [a]) :< fs) => Loc a -> a -> Eff fs ()
ext' loc val = modify (Map.insertWith (<>) loc [val])
