{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Interpreter.Collecting
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Abstract.Type
import Abstract.Syntax
import Abstract.Value
import Control.Applicative
import Control.Effect
import Control.Monad.Effect.Failure
import Control.Monad.Effect.Internal hiding (run)
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable
import Data.Function (fix)
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Maybe
import Data.Pointed
import Data.Semigroup
import Data.Text.Prettyprint.Doc

newtype Cache l t v = Cache { unCache :: Map.Map (Configuration l t v) (Set (v, Store l v)) }

deriving instance (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Monoid (Cache l t v)

cacheLookup :: (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Configuration l t v -> Cache l t v -> Maybe (Set (v, Store l v))
cacheLookup key = Map.lookup key . unCache

cacheSet :: (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Configuration l t v -> Set (v, Store l v) -> Cache l t v -> Cache l t v
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Configuration l t v -> (v, Store l v) -> Cache l t v -> Cache l t v
cacheInsert = (((Cache .) . (. unCache)) .) . (. point) . Map.insertWith (<>)


type CachingInterpreter l t v = '[Fresh, Reader (Set (Address l v)), Reader (Environment l v), Failure, NonDetEff, State (Store l v), Reader (Cache l t v), State (Cache l t v)]

type CachingResult l t v = Final (CachingInterpreter l t v) v

type MonadCachingInterpreter l t v m = (MonadEnv l v m, MonadStore l v m, MonadCacheIn l t v m, MonadCacheOut l t v m, MonadGC l v m, Alternative m)


class Monad m => MonadCacheIn l t v m where
  askCache :: m (Cache l t v)
  localCache :: (Cache l t v -> Cache l t v) -> m a -> m a

instance Reader (Cache l t v) :< fs => MonadCacheIn l t v (Eff fs) where
  askCache = ask
  localCache = local


class Monad m => MonadCacheOut l t v m where
  getCache :: m (Cache l t v)
  putCache :: Cache l t v -> m ()

instance State (Cache l t v) :< fs => MonadCacheOut l t v (Eff fs) where
  getCache = get
  putCache = put

modifyCache :: MonadCacheOut l t v m => (Cache l t v -> Cache l t v) -> m ()
modifyCache f = fmap f getCache >>= putCache


class (Alternative m, Monad m) => MonadNonDet m where
  collect :: Monoid b => (a -> b) -> m a -> m b

instance NonDetEff :< fs => MonadNonDet (Eff fs) where
  collect f = interpose (pure . f) (\ m k -> case m of
    MZero -> pure mempty
    MPlus -> mappend <$> k True <*> k False)


-- Coinductively-cached evaluation

evalCache :: forall l v a
          .  (Ord a, Ord v, Ord l, Ord1 (Cell l), Foldable (Cell l), MonadAddress l (Eff (CachingInterpreter l (Term Prim) v)), MonadValue l v (Term Prim) (Eff (CachingInterpreter l (Term Prim) v)), MonadPrim v (Eff (CachingInterpreter l (Term Prim) v)), Semigroup (Cell l v))
          => Eval (Term Prim) (CachingResult l (Term Prim) v)
evalCache = run @(CachingInterpreter l (Term Prim) v) . fixCache @l (fix (evCache @l (evCollect (evRoots (ev @l)))))

evCache :: forall l t v m
        .  (Ord l, Ord t, Ord v, Ord1 (Cell l), MonadCachingInterpreter l t v m)
        => (Eval t (m v) -> Eval t (m v))
        -> Eval t (m v)
        -> Eval t (m v)
evCache ev0 ev e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  let c = Configuration e roots env store :: Configuration l t v
  out <- getCache
  case cacheLookup c out of
    Just pairs -> asum . flip map (toList pairs) $ \ (value, store') -> do
      putStore store'
      return value
    Nothing -> do
      in' <- askCache
      let pairs = fromMaybe mempty (cacheLookup c in')
      putCache (cacheSet c pairs out)
      v <- ev0 ev e
      store' <- getStore
      modifyCache (cacheInsert c (v, store'))
      return v

fixCache :: forall l t v m
         .  (Ord l, Ord t, Ord v, Ord1 (Cell l), MonadCachingInterpreter l t v m, MonadNonDet m)
         => Eval t (m v)
         -> Eval t (m v)
fixCache eval e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  let c = Configuration e roots env store :: Configuration l t v
  pairs <- mlfp mempty (\ dollar -> do
    putCache (mempty :: Cache l t v)
    putStore store
    _ <- localCache (const dollar) (collect point (eval e) :: m (Set v))
    getCache)
  asum . flip map (maybe [] toList (cacheLookup c pairs)) $ \ (value, store') -> do
    putStore store'
    return value


mlfp :: (Eq a, Monad m) => a -> (a -> m a) -> m a
mlfp a f = loop a
  where loop x = do
          x' <- f x
          if x' == x then
            return x
          else
            loop x'


instance (Eq l, Eq1 (Cell l)) => Eq2 (Cache l) where
  liftEq2 eqT eqV (Cache a) (Cache b) = liftEq2 (liftEq2 eqT eqV) (liftEq (liftEq2 eqV (liftEq eqV))) a b

instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Cache l t) where
  liftEq = liftEq2 (==)

instance (Eq l, Eq t, Eq v, Eq1 (Cell l)) => Eq (Cache l t v) where
  (==) = eq1


instance (Ord l, Ord1 (Cell l)) => Ord2 (Cache l) where
  liftCompare2 compareT compareV (Cache a) (Cache b) = liftCompare2 (liftCompare2 compareT compareV) (liftCompare (liftCompare2 compareV (liftCompare compareV))) a b

instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Cache l t) where
  liftCompare = liftCompare2 compare

instance (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Ord (Cache l t v) where
  compare = compare1


instance (Show l, Show1 (Cell l)) => Show2 (Cache l) where
  liftShowsPrec2 spT slT spV slV d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
    where spKey = liftShowsPrec2 spT slT spV slV
          slKey = liftShowList2 spT slT spV slV
          spPair = liftShowsPrec2 spV slV spStore slStore
          slPair = liftShowList2 spV slV spStore slStore
          spStore = liftShowsPrec spV slV
          slStore = liftShowList  spV slV

instance (Show l, Show t, Show1 (Cell l)) => Show1 (Cache l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show l, Show t, Show v, Show1 (Cell l)) => Show (Cache l t v) where
  showsPrec = showsPrec1


instance (Pretty l, Pretty1 (Cell l)) => Pretty2 (Cache l) where
  liftPretty2 pT plT pV plV = list . map (liftPretty2 prettyConfiguration prettyListConfiguration prettySet prettyListSet) . Map.toList . unCache
    where prettyConfiguration = liftPretty2 pT plT pV plV
          prettyListConfiguration = list . map (liftPretty2 pT plT pV plV)
          prettyPair = liftPretty2 pV plV prettyStore prettyListStore
          prettyListPair = list . map prettyPair
          prettySet = liftPretty prettyPair prettyListPair
          prettyListSet = list . map prettySet
          prettyStore = liftPretty pV plV
          prettyListStore = list . map (liftPretty pV plV)

instance (Pretty l, Pretty t, Pretty1 (Cell l)) => Pretty1 (Cache l t) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty l, Pretty t, Pretty v, Pretty1 (Cell l)) => Pretty (Cache l t v) where
  pretty = liftPretty pretty prettyList
