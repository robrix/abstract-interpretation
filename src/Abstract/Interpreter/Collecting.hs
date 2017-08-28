{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Collecting where

import Abstract.RootSet
import Control.Monad.Effect
import Control.Monad.Effect.Reader

class MonadGC l a m where
  askRoots :: m (Roots l a)

instance Reader (Roots l a) :< fs => MonadGC l a (Eff fs) where
  askRoots = ask
