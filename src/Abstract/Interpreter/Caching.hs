{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect.Amb
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable
import Data.Function (fix)
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc

newtype Cache l t v = Cache { unCache :: Map.Map (Configuration l t v) (Set.Set (v, Store l v)) }
  deriving (Monoid)

cacheLookup :: (Ord t, Ord v, Address l) => Configuration l t v -> Cache l t v -> Maybe (Set.Set (v, Store l v))
cacheLookup = (. unCache) . Map.lookup

cacheSet :: (Ord t, Ord v, Address l) => Configuration l t v -> Set.Set (v, Store l v) -> Cache l t v -> Cache l t v
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord t, Ord v, Address l) => Configuration l t v -> (v, Store l v) -> Cache l t v -> Cache l t v
cacheInsert = (((Cache .) . (. unCache)) .) . (. Set.singleton) . Map.insertWith (<>)


type CachingInterpreter l t v = Amb ': State (Cache l t v) ': Reader (Cache l t v) ': Interpreter l v

type CachingResult l t v = (Either String ([] v, Cache l t v), Store l v)


-- Coinductively-cached evaluation

evalCache :: forall l a . (Ord a, Address l, Context l (Value l (Term a) a) (Eff (CachingInterpreter l (Term a) (Value l (Term a) a))), PrimitiveOperations a (Eff (CachingInterpreter l (Term a) (Value l (Term a) a)))) => Term a -> CachingResult l (Term a) (Value l (Term a) a)
evalCache = run @(CachingInterpreter l (Term a) (Value l (Term a) a)) . runCache (Proxy :: Proxy l) ev

runCache :: (Ord t, Ord v, Address l, Context l v (Eff fs), CachingInterpreter l t v :<: fs)
         => proxy l
         -> (Eval t (Eff fs v) -> Eval t (Eff fs v))
         -> Eval t (Eff fs v)
runCache proxy ev = fixCache proxy (fix (evCache proxy ev))

evCache :: forall l t v fs proxy
        .  (Ord t, Ord v, Address l, Context l v (Eff fs), CachingInterpreter l t v :<: fs)
        => proxy l
        -> (Eval t (Eff fs v) -> Eval t (Eff fs v))
        -> Eval t (Eff fs v)
        -> Eval t (Eff fs v)
evCache _ ev0 ev e = do
  env <- ask
  store <- get
  let c = Configuration e (env :: Environment (l v)) store :: Configuration l t v
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

fixCache :: forall l t fs v proxy
         .  (Ord t, Ord v, Address l, Context l v (Eff fs), CachingInterpreter l t v :<: fs)
         => proxy l
         -> Eval t (Eff fs v)
         -> Eval t (Eff fs v)
fixCache _ eval e = do
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


instance Address l => Eq2 (Cache l) where
  liftEq2 eqT eqV (Cache a) (Cache b) = liftEq2 (liftEq2 eqT eqV) (liftEq (liftEq2 eqV (liftEq eqV))) a b

instance (Eq t, Address l) => Eq1 (Cache l t) where
  liftEq = liftEq2 (==)

instance (Eq v, Eq t, Address l) => Eq (Cache l t v) where
  (==) = eq1


instance Address l => Show2 (Cache l) where
  liftShowsPrec2 spT slT spV slV d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
    where spKey = liftShowsPrec2 spT slT spV slV
          slKey = liftShowList2 spT slT spV slV
          spPair = liftShowsPrec2 spV slV spStore slStore
          slPair = liftShowList2 spV slV spStore slStore
          spStore = liftShowsPrec spV slV
          slStore = liftShowList  spV slV

instance (Show t, Address l) => Show1 (Cache l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show t, Address l) => Show (Cache l t a) where
  showsPrec = showsPrec1


instance (Address l, Pretty1 l, Pretty1 (Cell l)) => Pretty2 (Cache l) where
  liftPretty2 pT plT pV plV = list . map (liftPretty2 prettyConfiguration prettyListConfiguration prettySet prettyListSet) . Map.toList . unCache
    where prettyConfiguration = liftPretty2 pT plT pV plV
          prettyListConfiguration = list . map (liftPretty2 pT plT pV plV)
          prettySet = list . map (liftPretty2 pV plV prettyStore prettyListStore) . Set.toList
          prettyListSet = list . map prettySet
          prettyStore = liftPretty pV plV
          prettyListStore = list . map (liftPretty pV plV)

instance (Address l, Pretty1 l, Pretty1 (Cell l), Pretty t) => Pretty1 (Cache l t) where
  liftPretty = liftPretty2 pretty prettyList

instance (Address l, Pretty1 l, Pretty1 (Cell l), Pretty t, Pretty v) => Pretty (Cache l t v) where
  pretty = liftPretty pretty prettyList
