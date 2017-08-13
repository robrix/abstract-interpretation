{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Abstract.Store where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Foldable (asum)
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Semigroup

type Store f a = Map.Map (Loc a) (f a)

newtype Loc a = Loc { unLoc :: String }
  deriving (Eq, Ord, Show)

data AbstractStore f fs a = AbstractStore
  { find :: (State (Store f a) :< fs) => Loc a -> Eff fs a
  , alloc :: (State (Store f a) :< fs) => String -> Eff fs (Loc a)
  , ext :: (State (Store f a) :< fs) => Loc a -> a -> Eff fs ()
  }

preciseStore :: Ord a => AbstractStore Identity fs a
preciseStore = AbstractStore
  { find = fmap runIdentity . flip fmap get . flip (Map.!)
  , alloc = return . Loc
  , ext = \ loc val -> modify (Map.insert loc (Identity val))
  }

abstractStore :: forall a fs. (Ord a, Alternative (Eff fs)) => AbstractStore [] fs a
abstractStore = AbstractStore
  { find = \ loc -> do
    store <- get
    asum (return <$> ((store :: Store [] a) Map.! loc))
  , alloc = return . Loc
  , ext = \ loc val -> modify (Map.insertWith (<>) loc [val])
  }
