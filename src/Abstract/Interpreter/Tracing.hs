{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Tracing where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Interpreter.Collecting
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Writer
import Data.Function (fix)
import Data.Functor.Classes (Ord1)
import Data.Semigroup
import qualified Data.Set as Set
import GHC.Exts (IsList(..))

type TracingInterpreter l t v g = Reader (Set (Address l v)) ': Writer (g (Configuration l t v)) ': Interpreter l v

type TraceInterpreter l t v = TracingInterpreter l t v []
type ReachableStateInterpreter l t v = TracingInterpreter l t v Set.Set

type TraceResult l t v f = Final (TracingInterpreter l t v f) v


class Monad m => MonadTrace l t v g m where
  trace :: g (Configuration l t v) -> m ()

instance Writer (g (Configuration l t v)) :< fs => MonadTrace l t v g (Eff fs) where
  trace = tell


-- Tracing and reachable state analyses

evalTrace :: forall l v
          .  (MonadAddress l (Eff (TraceInterpreter l (Term Prim) v)), MonadValue l v (Term Prim) (Eff (TraceInterpreter l (Term Prim) v)), MonadPrim v (Eff (TraceInterpreter l (Term Prim) v)), Semigroup (Cell l v))
          => Eval (Term Prim) (TraceResult l (Term Prim) v [])
evalTrace = run @(TraceInterpreter l (Term Prim) v) . fix (evTell @l @(Term Prim) @v @[] (ev @l))

evalReach :: forall l v a
          .  (Ord a, Ord v, Ord l, Ord1 (Cell l), MonadAddress l (Eff (ReachableStateInterpreter l (Term Prim) v)), MonadValue l v (Term Prim) (Eff (ReachableStateInterpreter l (Term Prim) v)), MonadPrim v (Eff (ReachableStateInterpreter l (Term Prim) v)), Semigroup (Cell l v))
          => Eval (Term Prim) (TraceResult l (Term Prim) v Set.Set)
evalReach = run @(ReachableStateInterpreter l (Term Prim) v) . fix (evTell @l @(Term Prim) @v @Set.Set (ev @l))

evTell :: forall l t v g m
       .  (Ord l, IsList (g (Configuration l t v)), Item (g (Configuration l t v)) ~ Configuration l t v, MonadTrace l t v g m, MonadEnv l v m, MonadStore l v m, MonadGC l v m)
       => (Eval t (m v) -> Eval t (m v))
       -> Eval t (m v)
       -> Eval t (m v)
evTell ev0 ev e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  trace (fromList [Configuration e roots env store] :: g (Configuration l t v))
  ev0 ev e
