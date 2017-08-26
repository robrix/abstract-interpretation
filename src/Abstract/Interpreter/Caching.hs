{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Failure
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable
import Data.Function (fix)
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc

newtype Cache l t v = Cache { unCache :: Map.Map (Configuration l t v) (Set.Set (v, Store l v)) }

deriving instance (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Monoid (Cache l t v)

cacheLookup :: (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Configuration l t v -> Cache l t v -> Maybe (Set.Set (v, Store l v))
cacheLookup = (. unCache) . Map.lookup

cacheSet :: (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Configuration l t v -> Set.Set (v, Store l v) -> Cache l t v -> Cache l t v
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Configuration l t v -> (v, Store l v) -> Cache l t v -> Cache l t v
cacheInsert = (((Cache .) . (. unCache)) .) . (. Set.singleton) . Map.insertWith (<>)


type CachingInterpreter l t v = '[Reader (Environment (Address l v)), Failure, NonDetEff, State (Store l v), Reader (Cache l t v), State (Cache l t v)]

type CachingResult l t v = Final (CachingInterpreter l t v) v


-- Coinductively-cached evaluation

evalCache :: forall l v a
          .  (Ord a, Ord v, Ord l, Ord1 (Cell l), AbstractAddress l (CachingInterpreter l (Term a) v), AbstractValue l v Term a, PrimitiveOperations v (CachingInterpreter l (Term a) v))
          => Eval (Term a) (CachingResult l (Term a) v)
evalCache = run @(CachingInterpreter l (Term a) v) . runCache @l (ev @l)

runCache :: forall l t v fs
         .  (CachingInterpreter l t v :<: fs, Ord l, Ord t, Ord v, Ord1 (Cell l), AbstractAddress l fs)
         => (Eval t (Eff fs v) -> Eval t (Eff fs v))
         -> Eval t (Eff fs v)
runCache ev = fixCache @l (fix (evCache @l ev))

evCache :: forall l t v fs
        .  (CachingInterpreter l t v :<: fs, Ord l, Ord t, Ord v, Ord1 (Cell l), AbstractAddress l fs)
        => (Eval t (Eff fs v) -> Eval t (Eff fs v))
        -> Eval t (Eff fs v)
        -> Eval t (Eff fs v)
evCache ev0 ev e = do
  env <- ask
  store <- get
  let c = Configuration e (env :: Environment (Address l v)) store :: Configuration l t v
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

fixCache :: forall l t fs v
         .  (Ord l, Ord t, Ord v, Ord1 (Cell l), AbstractAddress l fs, CachingInterpreter l t v :<: fs)
         => Eval t (Eff fs v)
         -> Eval t (Eff fs v)
fixCache eval e = do
  env <- ask
  store <- get
  let c = Configuration e env store :: Configuration l t v
  pairs <- mlfp mempty (\ dollar -> do
    putCache (mempty :: Cache l t v)
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


askCache :: (Reader (Cache l t v) :< fs) => Eff fs (Cache l t v)
askCache = ask

localCache :: (Reader (Cache l t v) :< fs) => (Cache l t v -> Cache l t v) -> Eff fs b -> Eff fs b
localCache = local


getCache :: (State (Cache l t v) :< fs) => Eff fs (Cache l t v)
getCache = get

putCache :: (State (Cache l t v) :< fs) => Cache l t v -> Eff fs ()
putCache = put

modifyCache :: (State (Cache l t v) :< fs) => (Cache l t v -> Cache l t v) -> Eff fs ()
modifyCache f = fmap f getCache >>= putCache


instance (Eq l, Eq1 (Cell l)) => Eq2 (Cache l) where
  liftEq2 eqT eqV (Cache a) (Cache b) = liftEq2 (liftEq2 eqT eqV) (liftEq (liftEq2 eqV (liftEq eqV))) a b

instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Cache l t) where
  liftEq = liftEq2 (==)

instance (Eq l, Eq t, Eq v, Eq1 (Cell l)) => Eq (Cache l t v) where
  (==) = eq1


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
          prettySet = list . map (liftPretty2 pV plV prettyStore prettyListStore) . Set.toList
          prettyListSet = list . map prettySet
          prettyStore = liftPretty pV plV
          prettyListStore = list . map (liftPretty pV plV)

instance (Pretty l, Pretty t, Pretty1 (Cell l)) => Pretty1 (Cache l t) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty l, Pretty t, Pretty v, Pretty1 (Cell l)) => Pretty (Cache l t v) where
  pretty = liftPretty pretty prettyList
