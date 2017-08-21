{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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

newtype Cache l a = Cache { unCache :: Map.Map (Configuration l (Term a) a) (Set.Set (Value l (Term a) a, AddressStore l (Value l (Term a) a))) }
  deriving (Monoid)

cacheLookup :: (Ord a, Address l) => Configuration l (Term a) a -> Cache l a -> Maybe (Set.Set (Value l (Term a) a, AddressStore l (Value l (Term a) a)))
cacheLookup = (. unCache) . Map.lookup

cacheSet :: (Ord a, Ord (AddressStore l (Value l (Term a) a)), Address l) => Configuration l (Term a) a -> Set.Set (Value l (Term a) a, AddressStore l (Value l (Term a) a)) -> Cache l a -> Cache l a
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord a, Ord (AddressStore l (Value l (Term a) a)), Address l) => Configuration l (Term a) a -> (Value l (Term a) a, AddressStore l (Value l (Term a) a)) -> Cache l a -> Cache l a
cacheInsert = (((Cache .) . (. unCache)) .) . (. Set.singleton) . Map.insertWith (<>)


type CachingInterpreter l a = Amb ': State (Cache l a) ': Reader (Cache l a) ': Interpreter l a

type CachingResult l a = (Either String ([] (Value l (Term a) a), Cache l a), AddressStore l (Value l (Term a) a))


-- Coinductively-cached evaluation

evalCache :: forall l a . (Ord a, Ord (l (Value l (Term a) a)), Ord (AddressStore l (Value l (Term a) a)), Monoid (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) (CachingInterpreter l a), AbstractNumber a (Eff (CachingInterpreter l a))) => Term a -> CachingResult l a
evalCache = run @(CachingInterpreter l a) . runCache

runCache :: (Ord a, Ord (l (Value l (Term a) a)), Ord (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) fs, AbstractNumber a (Eff fs), CachingInterpreter l a :<: fs) => Eval l (Term a) fs a
runCache = fixCache (fix (evCache ev))

evCache :: forall l a fs
        .  (Ord a, Ord (l (Value l (Term a) a)), Ord (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) fs, CachingInterpreter l a :<: fs)
        => (Eval l (Term a) fs a -> Eval l (Term a) fs a)
        -> Eval l (Term a) fs a
        -> Eval l (Term a) fs a
evCache ev0 ev e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l (Term a) a
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
         .  (Ord a, Ord (l (Value l (Term a) a)), Ord (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) fs, CachingInterpreter l a :<: fs)
         => Eval l (Term a) fs a
         -> Eval l (Term a) fs a
fixCache eval e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l (Term a) a
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


askCache :: (Reader (Cache l a) :< fs) => Eff fs (Cache l a)
askCache = ask

localCache :: (Reader (Cache l a) :< fs) => (Cache l a -> Cache l a) -> Eff fs b -> Eff fs b
localCache = local


getCache :: (State (Cache l a) :< fs) => Eff fs (Cache l a)
getCache = get

putCache :: (State (Cache l a) :< fs) => Cache l a -> Eff fs ()
putCache = put

modifyCache :: (State (Cache l a) :< fs) => (Cache l a -> Cache l a) -> Eff fs ()
modifyCache f = fmap f getCache >>= putCache


instance Address l => Eq1 (Cache l) where
  liftEq eq (Cache a) (Cache b) = liftEq2 (liftEq2 (liftEq eq) eq) (liftEq (liftEq2 eqValue (liftEq eqValue))) a b
    where eqValue = liftEq2 (liftEq eq) eq

instance (Eq a, Address l) => Eq (Cache l a) where
  (==) = eq1


instance Address l => Show1 (Cache l) where
  liftShowsPrec sp sl d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
    where spKey = liftShowsPrec2 (liftShowsPrec sp sl) (liftShowList sp sl) sp sl
          slKey = liftShowList2 (liftShowsPrec sp sl) (liftShowList sp sl) sp sl
          spPair = liftShowsPrec2 spValue slValue spStore slStore
          slPair = liftShowList2 spValue slValue spStore slStore
          spStore = liftShowsPrec spValue slValue
          slStore = liftShowList  spValue slValue
          spValue = liftShowsPrec2 (liftShowsPrec sp sl) (liftShowList sp sl) sp sl
          slValue = liftShowList2 (liftShowsPrec sp sl) (liftShowList sp sl) sp sl

instance (Show a, Address l) => Show (Cache l a) where
  showsPrec = showsPrec1
