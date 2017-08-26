{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Trace where

import Abstract.Configuration
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Writer
import Data.Function (fix)
import Data.Functor.Classes (Ord1)
import qualified Data.Set as Set
import GHC.Exts (IsList(..))

type TracingInterpreter l t v g = Writer (g (Configuration l t v)) ': Interpreter l v

type TraceInterpreter l t v = TracingInterpreter l t v []
type ReachableStateInterpreter l t v = TracingInterpreter l t v Set.Set

type TraceResult l t v f = Final (TracingInterpreter l t v f) v


-- Tracing and reachable state analyses

evalTrace :: forall l v a
          .  (AbstractAddress l (Eff (TraceInterpreter l (Term a) v)), AbstractValue l v Term a, PrimitiveOperations v (TraceInterpreter l (Term a) v))
          => Eval (Term a) (TraceResult l (Term a) v [])
evalTrace = run @(TraceInterpreter l (Term a) v) . runTrace @l (ev @l)

runTrace :: forall l t v fs
         .  (TraceInterpreter l t v :<: fs)
         => (Eval t (Eff fs v) -> Eval t (Eff fs v))
         -> Eval t (Eff fs v)
runTrace ev = fix (evTell @l @t @v @[] ev)

evalReach :: forall l v a
          .  (Ord a, Ord v, Ord l, Ord1 (Cell l), AbstractAddress l (Eff (ReachableStateInterpreter l (Term a) v)), AbstractValue l v Term a, PrimitiveOperations v (ReachableStateInterpreter l (Term a) v))
          => Eval (Term a) (TraceResult l (Term a) v Set.Set)
evalReach = run @(ReachableStateInterpreter l (Term a) v) . runReach @l (ev @l)

runReach :: forall l t v fs
         .  (ReachableStateInterpreter l t v :<: fs, Ord t, Ord v, Ord l, Ord1 (Cell l))
         => (Eval t (Eff fs v) -> Eval t (Eff fs v))
         -> Eval t (Eff fs v)
runReach ev = fix (evTell @l @t @v @Set.Set ev)

evTell :: forall l t v g fs
       .  (TracingInterpreter l t v g :<: fs, IsList (g (Configuration l t v)), Item (g (Configuration l t v)) ~ Configuration l t v)
       => (Eval t (Eff fs v) -> Eval t (Eff fs v))
       -> Eval t (Eff fs v)
       -> Eval t (Eff fs v)
evTell ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [Configuration e env store] :: g (Configuration l t v))
  ev0 ev e
