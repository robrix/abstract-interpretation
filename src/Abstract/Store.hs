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
  , ext = (modify .) . (. Identity) . Map.insert
  }

find' :: forall a fs. (Alternative (Eff fs), State (Store [] a) :< fs) => Loc a -> Eff fs a
find' loc = do
  store <- get
  asum (return <$> ((store :: Store [] a) Map.! loc))

alloc' :: Alternative m => String -> m (Loc a)
alloc' x = pure (Loc x)

ext' :: (State (Store [] a) :< fs) => Loc a -> a -> Eff fs ()
ext' loc val = modify (Map.insertWith (<>) loc [val])
