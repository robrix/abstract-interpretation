{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Trace where

import Abstract.Interpreter
import Abstract.Number
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Writer
import Data.Function (fix)
import qualified Data.Set as Set
import GHC.Exts (IsList(..))

type TraceEntry l i = (Term i, Environment (l (Value l i)), Store l (Value l i))
type Trace l i g = g (TraceEntry l i)

type TracingInterpreter l i g = Writer (Trace l i g) ': Interpreter l i

type TraceInterpreter l i = TracingInterpreter l i []
type ReachableStateInterpreter l i = TracingInterpreter l i Set.Set


-- Tracing and reachable state analyses

evalTrace :: forall i l. (Monoid (Store l (Value l i)), AbstractStore l, Context l (Value l i) (TraceInterpreter l i), AbstractNumber i (Eff (TraceInterpreter l i))) => Term i -> (Either String (Value l i, Trace l i []), Store l (Value l i))
evalTrace = run @(TraceInterpreter l i) . runTrace

runTrace :: (TraceInterpreter l i :<: fs, AbstractStore l, Context l (Value l i) fs, AbstractNumber i (Eff fs)) => Term i -> Eff fs (Value l i)
runTrace = fix (evTell [] ev)

evalReach :: forall i l. (Monoid (Store l (Value l i)), Ord i, Ord (l (Value l i)), Ord (Store l (Value l i)), AbstractStore l, Context l (Value l i) (ReachableStateInterpreter l i), AbstractNumber i (Eff (ReachableStateInterpreter l i))) => Term i -> (Either String (Value l i, Trace l i Set.Set), Store l (Value l i))
evalReach = run @(ReachableStateInterpreter l i) . runReach

runReach :: (Ord i, Ord (l (Value l i)), Ord (Store l (Value l i)), ReachableStateInterpreter l i :<: fs, AbstractStore l, Context l (Value l i) fs, AbstractNumber i (Eff fs)) => Term i -> Eff fs (Value l i)
runReach = fix (evTell Set.empty ev)

evTell :: forall l i g fs . (TracingInterpreter l i g :<: fs, IsList (Trace l i g), Item (Trace l i g) ~ TraceEntry l i)
       => g ()
       -> ((Term i -> Eff fs (Value l i)) -> Term i -> Eff fs (Value l i))
       -> (Term i -> Eff fs (Value l i))
       -> Term i
       -> Eff fs (Value l i)
evTell _ ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [(e, env, store)] :: Trace l i g)
  ev0 ev e
