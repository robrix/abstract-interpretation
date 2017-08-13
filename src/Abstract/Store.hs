{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, InstanceSigs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Abstract.Store where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Foldable (asum)
import qualified Data.IntMap as IntMap
import Data.Kind
import qualified Data.Map as Map
import Data.Semigroup

newtype Precise a = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

newtype Monovariant a = Monovariant { unMonovariant :: String }
  deriving (Eq, Ord, Show)

class AbstractStore l where
  type Store l a
  type Context l a (fs :: [* -> *]) :: Constraint
  type instance Context l a fs = (State (Store l a) :< fs)

  find :: Context l a fs => l a -> Eff fs a

  alloc :: Context l a fs => String -> Eff fs (l a)

  ext :: Context l a fs => l a -> a -> Eff fs ()

instance AbstractStore Precise where
  type Store Precise a = IntMap.IntMap a
  find = flip fmap get . flip (IntMap.!) . unPrecise

  alloc :: forall a fs. (State (Store Precise a) :< fs) => String -> Eff fs (Precise a)
  alloc _ = do
    s <- get
    return (Precise (length (s :: Store Precise a)))

  ext (Precise loc) val = modify (IntMap.insert loc val)

instance AbstractStore Monovariant where
  type Store Monovariant a = Map.Map (Monovariant a) [a]
  type Context Monovariant a fs = (State (Store Monovariant a) :< fs, Alternative (Eff fs))

  find :: forall a fs. Context Monovariant a fs => Monovariant a -> Eff fs a
  find loc = do
    store <- get
    asum (return <$> ((store :: Store Monovariant a) Map.! loc))

  alloc x = pure (Monovariant x)

  ext loc val = modify (Map.insertWith (<>) loc [val])
