{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax hiding (subterms)
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Functor.Foldable
import Data.Semigroup
import qualified Data.Set as Set

type DeadCodeInterpreter l t v = State (Dead t) ': Interpreter l v

newtype Dead a = Dead { unDead :: Set.Set a }
  deriving (Eq, Foldable, Monoid, Ord, Show)

type DeadCodeResult l t v = Final (DeadCodeInterpreter l t v) v

class Monad m => MonadDead t m where
  killAll :: Dead t -> m ()
  revive :: Ord t => t -> m ()

instance State (Dead t) :< fs => MonadDead t (Eff fs) where
  killAll = put
  revive = modify . (Dead .) . (. unDead) . Set.delete


subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Set.Set a
subterms term = para (foldMap (uncurry ((<>) . Set.singleton))) term <> Set.singleton term


-- Dead code analysis

evalDead :: forall l v a. (AbstractAddress l (Eff (DeadCodeInterpreter l (Term a) v)), Ord a, AbstractValue l v Term a, MonadPrim v (Eff (DeadCodeInterpreter l (Term a) v))) => Eval (Term a) (DeadCodeResult l (Term a) v)
evalDead = run @(DeadCodeInterpreter l (Term a) v) . runDead (ev @l)

runDead :: forall t v m
        .  (Ord t, Recursive t, Foldable (Base t), MonadDead t m)
        => (Eval t (m v) -> Eval t (m v))
        -> Eval t (m v)
runDead ev e0 = do
  killAll (Dead (subterms e0))
  fix (evDead ev) e0

evDead :: (Ord t, MonadDead t m)
       => (Eval t (m v) -> Eval t (m v))
       -> Eval t (m v)
       -> Eval t (m v)
evDead ev0 ev e = do
  revive e
  ev0 ev e
