{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect.Internal
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as Set

type Cache l a = Map.Map (Configuration l a) (Set.Set (Value l a, Store l (Value l a)))

type CachingInterpreter l a = CacheOut l a ': CacheIn l a ': NonDetEff ': Interpreter l a


evCache :: forall l i fs
        .  (Ord i, Ord (l (Value l i)), Ord (Store l (Value l i)), AbstractStore l, Context l (Value l i) fs, CachingInterpreter l i :<: fs)
        => ((Term i -> Eff fs (Value l i)) -> Term i -> Eff fs (Value l i))
        -> (Term i -> Eff fs (Value l i))
        -> Term i
        -> Eff fs (Value l i)
evCache ev0 ev e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l i
  out <- getCacheOut
  case Map.lookup c out of
    Just pairs -> asum . flip map (Set.toList pairs) $ \ (value, store') -> do
      put store'
      return value
    Nothing -> do
      in' <- askCacheIn
      let pairs = fromMaybe Set.empty (Map.lookup c in')
      putCacheOut (Map.insert c pairs out)
      v <- ev0 ev e
      store' <- get
      let pair = (v, store')
      modifyCacheOut (Map.insertWith (<>) c (Set.singleton pair))
      return v

fixCache :: forall l a fs
         .  (Ord a, Ord (l (Value l a)), Ord (Store l (Value l a)), AbstractStore l, Context l (Value l a) fs, CachingInterpreter l a :<: fs)
         => (Term a -> Eff fs (Value l a))
         -> Term a
         -> Eff fs (Value l a)
fixCache eval e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l a
  pairs <- mlfp Map.empty (\ dollar -> do
    putCacheOut (Map.empty :: Cache l a)
    put store
    _ <- localCacheIn (const dollar) (eval e)
    getCacheOut)
  asum . flip map (Set.toList (fromMaybe Set.empty (Map.lookup c pairs))) $ \ (value, store') -> do
    put store'
    return value


mlfp :: (Eq a, Monad m) => a -> (a -> m a) -> m a
mlfp a f = loop a
  where loop x = do
          x' <- f x
          if x' == x then
            return x
          else
            loop x'


askCacheIn :: (CacheIn l a :< fs) => Eff fs (Cache l a)
askCacheIn = send Ask

localCacheIn :: forall l a fs b. (CacheIn l a :< fs) => (Cache l a -> Cache l a) -> Eff fs b -> Eff fs b
localCacheIn f m = do
  e <- fmap f askCacheIn
  let bind :: CacheIn l a v -> Arrow fs v b -> Eff fs b
      bind Ask g = g e
  interpose pure bind m


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
