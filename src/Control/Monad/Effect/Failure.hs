{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Failure where

import Control.Monad.Effect
import Control.Monad.Fail

data Failure a where
  Failure :: String -> Failure a

instance Failure :< fs => MonadFail (Eff fs) where
  fail = send . Failure
