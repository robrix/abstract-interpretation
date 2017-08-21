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

newtype Cache l t a = Cache { unCache :: Map.Map (Configuration l t a) (Set.Set (Value l t a, AddressStore l (Value l t a))) }
  deriving (Monoid)

cacheLookup :: (Ord a, Ord t, Address l) => Configuration l t a -> Cache l t a -> Maybe (Set.Set (Value l t a, AddressStore l (Value l t a)))
cacheLookup = (. unCache) . Map.lookup

cacheSet :: (Ord a, Ord t, Ord (AddressStore l (Value l t a)), Address l) => Configuration l t a -> Set.Set (Value l t a, AddressStore l (Value l t a)) -> Cache l t a -> Cache l t a
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord a, Ord t, Ord (AddressStore l (Value l t a)), Address l) => Configuration l t a -> (Value l t a, AddressStore l (Value l t a)) -> Cache l t a -> Cache l t a
cacheInsert = (((Cache .) . (. unCache)) .) . (. Set.singleton) . Map.insertWith (<>)


type CachingInterpreter l a = Amb ': State (Cache l (Term a) a) ': Reader (Cache l (Term a) a) ': Interpreter l a

type CachingResult l a = (Either String ([] (Value l (Term a) a), Cache l (Term a) a), AddressStore l (Value l (Term a) a))


-- Coinductively-cached evaluation

evalCache :: forall l a . (Ord a, Ord (l (Value l (Term a) a)), Ord (AddressStore l (Value l (Term a) a)), Monoid (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) (CachingInterpreter l a), AbstractNumber a (Eff (CachingInterpreter l a))) => Term a -> CachingResult l a
evalCache = run @(CachingInterpreter l a) . runCache

runCache :: (Ord a, Ord (l (Value l (Term a) a)), Ord (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) fs, AbstractNumber a (Eff fs), CachingInterpreter l a :<: fs) => Eval (Term a) l fs a
runCache = fixCache (fix (evCache ev))

evCache :: forall l a fs
        .  (Ord a, Ord (l (Value l (Term a) a)), Ord (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) fs, CachingInterpreter l a :<: fs)
        => (Eval (Term a) l fs a -> Eval (Term a) l fs a)
        -> Eval (Term a) l fs a
        -> Eval (Term a) l fs a
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
         => Eval (Term a) l fs a
         -> Eval (Term a) l fs a
fixCache eval e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l (Term a) a
  pairs <- mlfp mempty (\ dollar -> do
    putCache (mempty :: Cache l (Term a) a)
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


askCache :: (Reader (Cache l t a) :< fs) => Eff fs (Cache l t a)
askCache = ask

localCache :: (Reader (Cache l t a) :< fs) => (Cache l t a -> Cache l t a) -> Eff fs b -> Eff fs b
localCache = local


getCache :: (State (Cache l t a) :< fs) => Eff fs (Cache l t a)
getCache = get

putCache :: (State (Cache l t a) :< fs) => Cache l t a -> Eff fs ()
putCache = put

modifyCache :: (State (Cache l t a) :< fs) => (Cache l t a -> Cache l t a) -> Eff fs ()
modifyCache f = fmap f getCache >>= putCache


instance Address l => Eq2 (Cache l) where
  liftEq2 eqT eqA (Cache a) (Cache b) = liftEq2 (liftEq2 eqT eqA) (liftEq (liftEq2 eqValue (liftEq eqValue))) a b
    where eqValue = liftEq2 eqT eqA

instance (Eq t, Address l) => Eq1 (Cache l t) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq t, Address l) => Eq (Cache l t a) where
  (==) = eq1


instance Address l => Show2 (Cache l) where
  liftShowsPrec2 spT slT spA slA d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
    where spKey = liftShowsPrec2 spT slT spA slA
          slKey = liftShowList2 spT slT spA slA
          spPair = liftShowsPrec2 spValue slValue spStore slStore
          slPair = liftShowList2 spValue slValue spStore slStore
          spStore = liftShowsPrec spValue slValue
          slStore = liftShowList  spValue slValue
          spValue = liftShowsPrec2 spT slT spA slA
          slValue = liftShowList2 spT slT spA slA

instance (Show t, Address l) => Show1 (Cache l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show t, Address l) => Show (Cache l t a) where
  showsPrec = showsPrec1
