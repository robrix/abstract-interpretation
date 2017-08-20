{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Number
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect.Amb
import Control.Monad.Effect.Internal hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable
import Data.Function (fix)
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as Set

newtype Cache l a = Cache { unCache :: Map.Map (Configuration l a) (Set.Set (Value l a, AddressStore l (Value l a))) }
  deriving (Monoid)

cacheLookup :: (Ord a, Address l) => Configuration l a -> Cache l a -> Maybe (Set.Set (Value l a, AddressStore l (Value l a)))
cacheLookup = (. unCache) . Map.lookup

cacheSet :: (Ord a, Ord (AddressStore l (Value l a)), Address l) => Configuration l a -> Set.Set (Value l a, AddressStore l (Value l a)) -> Cache l a -> Cache l a
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord a, Ord (AddressStore l (Value l a)), Address l) => Configuration l a -> (Value l a, AddressStore l (Value l a)) -> Cache l a -> Cache l a
cacheInsert = (((Cache .) . (. unCache)) .) . (. Set.singleton) . Map.insertWith (<>)


type CachingInterpreter l a = Amb ': CacheState l a ': CacheReader l a ': Interpreter l a

type CachingResult l a = (Either String ([] (Value l a), Cache l a), AddressStore l (Value l a))


-- Coinductively-cached evaluation

evalCache :: forall l a . (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Monoid (AddressStore l (Value l a)), Address l, Context l (Value l a) (CachingInterpreter l a), AbstractNumber a (Eff (CachingInterpreter l a))) => Term a -> CachingResult l a
evalCache = run @(CachingInterpreter l a) . runCache

runCache :: (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Address l, Context l (Value l a) fs, AbstractNumber a (Eff fs), CachingInterpreter l a :<: fs) => Eval l fs a
runCache = fixCache (fix (evCache ev))

evCache :: forall l a fs
        .  (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Address l, Context l (Value l a) fs, CachingInterpreter l a :<: fs)
        => (Eval l fs a -> Eval l fs a)
        -> Eval l fs a
        -> Eval l fs a
evCache ev0 ev e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l a
  out <- getCache
  case cacheLookup c out of
    Just pairs -> asum . flip map (toList pairs) $ \ (value, store') -> do
      put store'
      return value
    Nothing -> do
      in' <- askCache
      let pairs = fromMaybe Set.empty (cacheLookup c in')
      putCache (cacheSet c pairs out)
      v <- ev0 ev e
      store' <- get
      modifyCache (cacheInsert c (v, store'))
      return v

fixCache :: forall l a fs
         .  (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Address l, Context l (Value l a) fs, CachingInterpreter l a :<: fs)
         => Eval l fs a
         -> Eval l fs a
fixCache eval e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l a
  pairs <- mlfp mempty (\ dollar -> do
    putCache (mempty :: Cache l a)
    put store
    _ <- localCache (const dollar) (eval e)
    getCache)
  asum . flip map (maybe [] toList (cacheLookup c pairs)) $ \ (value, store') -> do
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


askCache :: (CacheReader l a :< fs) => Eff fs (Cache l a)
askCache = send Ask

localCache :: forall l a fs b. (CacheReader l a :< fs) => (Cache l a -> Cache l a) -> Eff fs b -> Eff fs b
localCache f m = do
  e <- fmap f askCache
  let bind :: CacheReader l a v -> Arrow fs v b -> Eff fs b
      bind Ask g = g e
  interpose pure bind m


getCache :: (CacheState l a :< fs) => Eff fs (Cache l a)
getCache = send Get

putCache :: (CacheState l a :< fs) => Cache l a -> Eff fs ()
putCache s = send (Put s)

modifyCache :: (CacheState l a :< fs) => (Cache l a -> Cache l a) -> Eff fs ()
modifyCache f = fmap f getCache >>= putCache


data CacheReader l a v where
  Ask :: CacheReader l a (Cache l a)

data CacheState l a v where
  Get :: CacheState l a (Cache l a)
  Put :: !(Cache l a) -> CacheState l a ()

instance (Ord a, Address l) => RunEffect (CacheReader l a) b where
  runEffect = relay pure (\ Ask k -> k mempty)

instance (Ord a, Address l) => RunEffect (CacheState l a) v where
  type Result (CacheState l a) v = (v, Cache l a)
  runEffect = relayState mempty ((pure .) . flip (,)) $ \ state eff yield -> case eff of
    Get -> yield state state
    Put state' -> yield state' ()


instance Address l => Eq1 (Cache l) where
  liftEq eq (Cache a) (Cache b) = liftEq2 (liftEq eq) (liftEq (liftEq2 (liftEq eq) (liftEq (liftEq eq)))) a b

instance (Eq a, Address l) => Eq (Cache l a) where
  (==) = eq1


instance Address l => Show1 (Cache l) where
  liftShowsPrec sp sl d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
    where spKey = liftShowsPrec sp sl
          slKey = liftShowList sp sl
          spPair = liftShowsPrec2 spValue slValue spStore slStore
          slPair = liftShowList2 spValue slValue spStore slStore
          spStore = liftShowsPrec spValue slValue
          slStore = liftShowList  spValue slValue
          spValue = liftShowsPrec sp sl
          slValue = liftShowList sp sl

instance (Show a, Address l) => Show (Cache l a) where
  showsPrec = showsPrec1
