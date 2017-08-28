{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Collecting where

import Abstract.Interpreter
import Abstract.Set
import Abstract.Store
import Abstract.Value
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semigroup

class Monad m => MonadGC l a m where
  askRoots :: m (Set (Address l a))

instance Reader (Set (Address l a)) :< fs => MonadGC l a (Eff fs) where
  askRoots = ask


gc :: Ord l => Set (Address l a) -> Store l a -> Store l a
gc roots store = storeRestrict store (reachable roots store)

reachable :: Ord l => Set (Address l a) -> Store l a -> Set (Address l a)
reachable roots _ = roots


evCollect :: forall l t v m
          .  (Ord l, MonadStore l v m, MonadGC l v m, MonadValue l v t m)
          => (Eval t (m v) -> Eval t (m v))
          -> Eval t (m v)
          -> Eval t (m v)
evCollect ev0 ev e = do
  roots <- askRoots :: m (Set (Address l v))
  v <- ev0 ev e
  modifyStore (gc (roots <> valueRoots v))
  return v
