{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Store
import Abstract.Value
import Control.Monad.Effect.Internal
import qualified Data.Map as Map

type Cache l a = Map.Map (Configuration l a) (Value l a, Store l (Value l a))


getCacheOut :: (Caching l a :< fs) => Eff fs (Cache l a)
getCacheOut = send Get

putCacheOut :: (Caching l a :< fs) => Cache l a -> Eff fs ()
putCacheOut s = send (Put s)

modifyCacheOut :: (Caching l a :< fs) => (Cache l a -> Cache l a) -> Eff fs ()
modifyCacheOut f = fmap f getCacheOut >>= putCacheOut


data Caching l a v where
  Get :: Caching l a (Cache l a)
  Put :: !(Cache l a) -> Caching l a ()
