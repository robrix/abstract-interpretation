{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect where

import Control.Monad.Effect as Effect hiding (run)
import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Failure
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader as Reader
import Control.Monad.Effect.State as State
import Control.Monad.Effect.Writer as Writer

newtype Effects fs ffs a where
  Effects :: { runEff :: fs :<: ffs => Eff ffs a } -> Effects fs ffs a

run :: RunEffects fs => Eff fs a -> Final fs a
run = Effect.run . runEffects

class RunEffects fs where
  type Final fs a
  runEffects :: Eff fs a -> Eff '[] (Final fs a)

instance (RunEffect f1, RunEffects (f2 ': fs)) => RunEffects (f1 ': f2 ': fs) where
  type Final (f1 ': f2 ': fs) a = Final (f2 ': fs) (Result f1 a)
  runEffects = runEffects . runEffect

instance RunEffect f => RunEffects '[f] where
  type Final '[f] a = Result f a
  runEffects = runEffect

class RunEffect f where
  type Result f a
  type instance Result f a = a
  runEffect :: Eff (f ': fs) a -> Eff fs (Result f a)

instance Monoid b => RunEffect (State b) where
  type Result (State b) a = (a, b)
  runEffect = flip runState mempty

instance Monoid b => RunEffect (Reader b) where
  runEffect = flip runReader mempty

instance RunEffect Failure where
  type Result Failure a = Either String a
  runEffect = runFailure

instance Monoid w => RunEffect (Writer w) where
  type Result (Writer w) a = (a, w)
  runEffect = runWriter

instance RunEffect NonDetEff where
  type Result NonDetEff a = [a]
  runEffect = makeChoiceA

instance Functor (Effects fs ffs) where
  fmap f (Effects eff) = Effects (fmap f eff)

instance Applicative (Effects fs ffs) where
  pure a = Effects (pure a)

  Effects f <*> Effects a = Effects (f <*> a)
