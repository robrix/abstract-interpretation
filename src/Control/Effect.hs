{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect where

import Control.Monad.Effect as Effect hiding (run)
import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Failure
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader as Reader
import Control.Monad.Effect.State as State
import Control.Monad.Effect.Writer as Writer

run :: RunEffects fs a => Eff fs a -> Final fs a
run = Effect.run . runEffects

class RunEffects fs a where
  type Final fs a
  runEffects :: Eff fs a -> Eff '[] (Final fs a)

instance (RunEffect f1 a, RunEffects (f2 ': fs) (Result f1 a)) => RunEffects (f1 ': f2 ': fs) a where
  type Final (f1 ': f2 ': fs) a = Final (f2 ': fs) (Result f1 a)
  runEffects = runEffects . runEffect

instance RunEffect f a => RunEffects '[f] a where
  type Final '[f] a = Result f a
  runEffects = runEffect

class RunEffect f a where
  type Result f a
  type instance Result f a = a
  runEffect :: Eff (f ': fs) a -> Eff fs (Result f a)

instance Monoid b => RunEffect (State b) a where
  type Result (State b) a = (a, b)
  runEffect = flip runState mempty

instance Monoid b => RunEffect (Reader b) a where
  runEffect = flip runReader mempty

instance RunEffect Failure a where
  type Result Failure a = Either String a
  runEffect = runFailure

instance Monoid w => RunEffect (Writer w) a where
  type Result (Writer w) a = (a, w)
  runEffect = runWriter

instance RunEffect NonDetEff a where
  type Result NonDetEff a = [a]
  runEffect = makeChoiceA
