{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect where

import Control.Monad.Effect as Effect hiding (run)
import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Failure
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader as Reader
import Control.Monad.Effect.State as State
import Control.Monad.Effect.Writer as Writer

run :: Effects fs => Eff fs a -> Final fs a
run = Effect.run . runEffects

class Effects fs where
  type Final fs a
  runEffects :: Eff fs a -> Eff '[] (Final fs a)

instance (Effect f1, Effects (f2 ': fs)) => Effects (f1 ': f2 ': fs) where
  type Final (f1 ': f2 ': fs) a = Final (f2 ': fs) (Result f1 a)
  runEffects = runEffects . runEffect

instance Effect f => Effects '[f] where
  type Final '[f] a = Result f a
  runEffects = runEffect

class Effect f where
  type Result f a
  type instance Result f a = a
  runEffect :: Eff (f ': fs) a -> Eff fs (Result f a)

instance Monoid b => Effect (State b) where
  type Result (State b) a = (a, b)
  runEffect = flip runState mempty

instance Monoid b => Effect (Reader b) where
  runEffect = flip runReader mempty

instance Effect Failure where
  type Result Failure a = Either String a
  runEffect = runFailure

instance Monoid w => Effect (Writer w) where
  type Result (Writer w) a = (a, w)
  runEffect = runWriter

instance Effect NonDetEff where
  type Result NonDetEff a = [a]
  runEffect = makeChoiceA
