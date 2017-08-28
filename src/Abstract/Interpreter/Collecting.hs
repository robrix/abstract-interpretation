{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Collecting where

import Abstract.Interpreter
import Abstract.RootSet
import Control.Monad.Effect
import Control.Monad.Effect.Reader

class Monad m => MonadGC l a m where
  askRoots :: m (RootSet l a)

instance Reader (RootSet l a) :< fs => MonadGC l a (Eff fs) where
  askRoots = ask


evCollect :: forall l t v m
          .  MonadGC l v m
          => (Eval t (m v) -> Eval t (m v))
          -> Eval t (m v)
          -> Eval t (m v)
evCollect ev0 ev e = do
  _ <- askRoots :: m (RootSet l v)
  v <- ev0 ev e
  return v
