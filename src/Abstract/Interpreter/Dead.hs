{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
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

revive :: Ord a => a -> Dead a -> Dead a
revive = (Dead .) . (. unDead) . Set.delete

type DeadCodeResult l t v = Final (DeadCodeInterpreter l t v) v


subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Set.Set a
subterms term = para (foldMap (uncurry ((<>) . Set.singleton))) term <> Set.singleton term


-- Dead code analysis

evalDead :: forall l v a. (AbstractAddress l (Eff (DeadCodeInterpreter l (Term a) v)), Ord a, AbstractValue l v Term a, PrimitiveOperations v (DeadCodeInterpreter l (Term a) v)) => Eval (Term a) (DeadCodeResult l (Term a) v)
evalDead = run @(DeadCodeInterpreter l (Term a) v) . runDead @l (ev @l)

runDead :: forall l t v fs
        .  (DeadCodeInterpreter l t v :<: fs, Ord t, Recursive t, Foldable (Base t))
        => (Eval t (Eff fs v) -> Eval t (Eff fs v))
        -> Eval t (Eff fs v)
runDead ev e0 = do
  put (Dead (subterms e0))
  fix (evDead @l ev) e0

evDead :: (DeadCodeInterpreter l t v :<: fs, Ord t)
       => (Eval t (Eff fs v) -> Eval t (Eff fs v))
       -> Eval t (Eff fs v)
       -> Eval t (Eff fs v)
evDead ev0 ev e = do
  modify (revive e)
  ev0 ev e
