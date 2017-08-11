{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Failure where

import Control.Monad.Effect
import Control.Monad.Effect.Internal
import Control.Monad.Fail

data Failure a where
  Failure :: String -> Failure a

runFailure :: Eff (Failure ': fs) a -> Eff fs (Either String a)
runFailure = relay (pure . Right) (\(Failure s) _ -> pure (Left s))

instance Failure :< fs => MonadFail (Eff fs) where
  fail = send . Failure
