{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeOperators #-}
module Abstract.Store where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Semigroup

type Store a = Map.Map (Loc a) a

newtype Loc a = Loc { unLoc :: String }
  deriving (Eq, Ord, Show)

find :: (State (Store a) :< fs) => Loc a -> Eff fs a
find = flip fmap get . flip (Map.!)

alloc :: forall a fs . (State (Store a) :< fs) => String -> Eff fs (Loc a)
alloc s = return (Loc s)

ext :: (State (Store a) :< fs) => Loc a -> a -> Eff fs ()
ext loc val = modify (Map.insert loc val)


type Store' a = Map.Map (Loc a) [a]

find' :: forall a fs. (Alternative (Eff fs), State (Store' a) :< fs) => Loc a -> Eff fs a
find' loc = do
  store <- get
  asum (return <$> ((store :: Store' a) Map.! loc))

alloc' :: Alternative m => String -> m (Loc a)
alloc' x = pure (Loc x)

ext' :: (State (Store' a) :< fs) => Loc a -> a -> Eff fs ()
ext' loc val = modify (Map.insertWith (<>) loc [val])
