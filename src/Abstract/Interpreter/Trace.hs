{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Trace where

import Abstract.Interpreter
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Writer
import Data.Constraint
import Data.Function (fix)
import qualified Data.Set as Set
import GHC.Exts (IsList(..))

type TraceEntry l i = (Term i, Environment (l i), Store l i)
type Trace l i g = g (TraceEntry l i)

type TracingInterpreter l i g = Writer (Trace l i g) ': Interpreter l i

type TraceInterpreter l i = TracingInterpreter l i []
type ReachableStateInterpreter l i = TracingInterpreter l i Set.Set


-- Tracing and reachable state analyses

evalTrace :: forall i l. (Monoid (Store l i), AbstractStore l, AbstractValue i (Eff (TraceInterpreter l i))) => Term i -> (Either String (Val l i, Trace l i []), Store l i)
evalTrace = run @(TraceInterpreter l i) . runTrace Dict

runTrace :: (TraceInterpreter l i :<: fs, AbstractStore l, AbstractValue i (Eff fs)) => Dict (AbstractStore l) -> Term i -> Eff fs (Val l i)
runTrace dict = fix (evTell dict [] (ev dict))

evalReach :: forall i l. (Monoid (Store l i), Ord i, Ord (l i), Ord (Store l i), AbstractStore l, AbstractValue i (Eff (ReachableStateInterpreter l i))) => Term i -> (Either String (Val l i, Trace l i Set.Set), Store l i)
evalReach = run @(ReachableStateInterpreter l i) . runReach Dict

runReach :: (Ord i, Ord (l i), Ord (Store l i), ReachableStateInterpreter l i :<: fs, AbstractStore l, AbstractValue i (Eff fs)) => Dict (AbstractStore l) -> Term i -> Eff fs (Val l i)
runReach dict = fix (evTell dict Set.empty (ev dict))

evTell :: forall l i g fs . (TracingInterpreter l i g :<: fs, IsList (Trace l i g), Item (Trace l i g) ~ TraceEntry l i)
       => Dict (AbstractStore l)
       -> g ()
       -> ((Term i -> Eff fs (Val l i)) -> Term i -> Eff fs (Val l i))
       -> (Term i -> Eff fs (Val l i))
       -> Term i
       -> Eff fs (Val l i)
evTell _ _ ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [(e, env, store)] :: Trace l i g)
  ev0 ev e
