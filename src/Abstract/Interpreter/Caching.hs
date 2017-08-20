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


type CachingInterpreter l a = Amb ': CacheOut l a ': CacheIn l a ': Interpreter l a

type CachingResult l a = (Either String ([] (Value l a), Cache l a), AddressStore l (Value l a))


-- Coinductively-cached evaluation

evalCache :: forall l a . (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Monoid (AddressStore l (Value l a)), Address l, Context l (Value l a) (CachingInterpreter l a), AbstractNumber a (Eff (CachingInterpreter l a))) => Term a -> CachingResult l a
evalCache = run @(CachingInterpreter l a) . runCache

runCache :: (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Address l, Context l (Value l a) fs, AbstractNumber a (Eff fs), CachingInterpreter l a :<: fs) => Term a -> Eff fs (Value l a)
runCache = fixCache (fix (evCache ev))

evCache :: forall l a fs
        .  (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Address l, Context l (Value l a) fs, CachingInterpreter l a :<: fs)
        => ((Term a -> Eff fs (Value l a)) -> Term a -> Eff fs (Value l a))
        -> (Term a -> Eff fs (Value l a))
        -> Term a
        -> Eff fs (Value l a)
evCache ev0 ev e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l a
  out <- getCacheOut
  case cacheLookup c out of
    Just pairs -> asum . flip map (toList pairs) $ \ (value, store') -> do
      put store'
      return value
    Nothing -> do
      in' <- askCacheIn
      let pairs = fromMaybe Set.empty (cacheLookup c in')
      putCacheOut (cacheSet c pairs out)
      v <- ev0 ev e
      store' <- get
      modifyCacheOut (cacheInsert c (v, store'))
      return v

fixCache :: forall l a fs
         .  (Ord a, Ord (l (Value l a)), Ord (AddressStore l (Value l a)), Address l, Context l (Value l a) fs, CachingInterpreter l a :<: fs)
         => (Term a -> Eff fs (Value l a))
         -> Term a
         -> Eff fs (Value l a)
fixCache eval e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l a
  pairs <- mlfp mempty (\ dollar -> do
    putCacheOut (mempty :: Cache l a)
    put store
    _ <- localCacheIn (const dollar) (eval e)
    getCacheOut)
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

instance (Ord a, Address l) => RunEffect (CacheIn l a) b where
  runEffect = relay pure (\ Ask k -> k mempty)

instance (Ord a, Address l) => RunEffect (CacheOut l a) v where
  type Result (CacheOut l a) v = (v, Cache l a)
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
