{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Collecting where

import Abstract.Interpreter
import Abstract.RootSet
import Abstract.Store
import Abstract.Value
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semigroup

class Monad m => MonadGC l a m where
  askRoots :: m (RootSet l a)

instance Reader (RootSet l a) :< fs => MonadGC l a (Eff fs) where
  askRoots = ask


gc :: RootSet l a -> Store l a -> Store l a
gc = const id


evCollect :: forall l t v m
          .  (Ord l, MonadStore l v m, MonadGC l v m, MonadValue l v t m)
          => (Eval t (m v) -> Eval t (m v))
          -> Eval t (m v)
          -> Eval t (m v)
evCollect ev0 ev e = do
  roots <- askRoots :: m (RootSet l v)
  v <- ev0 ev e
  modifyStore (gc (roots <> valueRoots v))
  return v
