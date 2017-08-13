{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Store
import Abstract.Value
import Control.Effect
import Control.Monad.Effect.Internal
import qualified Data.Map as Map

type Cache l a = Map.Map (Configuration l a) (Value l a, Store l (Value l a))


askCacheIn :: (Caching l a :< fs) => Eff fs (Cache l a)
askCacheIn = send Ask

getCacheOut :: (Caching l a :< fs) => Eff fs (Cache l a)
getCacheOut = send Get

putCacheOut :: (Caching l a :< fs) => Cache l a -> Eff fs ()
putCacheOut s = send (Put s)

modifyCacheOut :: (Caching l a :< fs) => (Cache l a -> Cache l a) -> Eff fs ()
modifyCacheOut f = fmap f getCacheOut >>= putCacheOut


data Caching l a v where
  Ask :: Caching l a (Cache l a)
  Get :: Caching l a (Cache l a)
  Put :: !(Cache l a) -> Caching l a ()

instance RunEffect (Caching l a) where
  type Result (Caching l a) v = (v, Cache l a)
  runEffect = relayState Map.empty ((pure .) . flip (,)) $ \ state eff yield -> case eff of
    Ask -> yield state state
    Get -> yield state state
    Put state' -> yield state' ()
