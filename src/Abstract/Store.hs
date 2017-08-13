{-# LANGUAGE FlexibleContexts, InstanceSigs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Abstract.Store where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Foldable (asum)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Semigroup

newtype Precise a = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

newtype Monovariant a = Monovariant { unMonovariant :: String }
  deriving (Eq, Ord, Show)

class AbstractStore l where
  type Store l a

  find :: (State (Store l a) :< fs) => l a -> Eff fs a

  alloc :: (State (Store l a) :< fs) => String -> Eff fs (l a)

  ext :: (State (Store l a) :< fs) => l a -> a -> Eff fs ()

instance AbstractStore Precise where
  type Store Precise a = IntMap.IntMap a
  find = flip fmap get . flip (IntMap.!) . unPrecise

  alloc :: forall a fs. (State (Store Precise a) :< fs) => String -> Eff fs (Precise a)
  alloc _ = do
    s <- get
    return (Precise (length (s :: Store Precise a)))

  ext (Precise loc) val = modify (IntMap.insert loc val)

find' :: forall a fs. (Alternative (Eff fs), State (Map.Map (Monovariant a) [a]) :< fs) => Monovariant a -> Eff fs a
find' loc = do
  store <- get
  asum (return <$> ((store :: Map.Map (Monovariant a) [a]) Map.! loc))

alloc' :: Alternative m => String -> m (Monovariant a)
alloc' x = pure (Monovariant x)

ext' :: (State (Map.Map (Monovariant a) [a]) :< fs) => Monovariant a -> a -> Eff fs ()
ext' loc val = modify (Map.insertWith (<>) loc [val])
