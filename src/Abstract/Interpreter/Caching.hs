{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect.Internal
import qualified Data.Map as Map

type Cache l a = Map.Map (Configuration l a) (Value l a, Store l (Value l a))

type CachingInterpreter l a = CacheOut l a ': CacheIn l a ': Interpreter l a

askCacheIn :: (CacheIn l a :< fs) => Eff fs (Cache l a)
askCacheIn = send Ask


getCacheOut :: (CacheOut l a :< fs) => Eff fs (Cache l a)
getCacheOut = send Get

putCacheOut :: (CacheOut l a :< fs) => Cache l a -> Eff fs ()
putCacheOut s = send (Put s)

modifyCacheOut :: (CacheOut l a :< fs) => (Cache l a -> Cache l a) -> Eff fs ()
modifyCacheOut f = fmap f getCacheOut >>= putCacheOut


data CacheIn l a v where
  Ask :: CacheIn l a (Cache l a)

data CacheOut l a v where
  Get :: CacheOut l a (Cache l a)
  Put :: !(Cache l a) -> CacheOut l a ()

instance RunEffect (CacheIn l a) where
  runEffect = relay pure (\ Ask k -> k Map.empty)

instance RunEffect (CacheOut l a) where
  type Result (CacheOut l a) v = (v, Cache l a)
  runEffect = relayState Map.empty ((pure .) . flip (,)) $ \ state eff yield -> case eff of
    Get -> yield state state
    Put state' -> yield state' ()
