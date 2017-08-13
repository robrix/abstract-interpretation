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
import Data.Function (fix)
import Data.Functor.Identity
import qualified Data.Set as Set
import GHC.Exts (IsList(..))

type TraceEntry i = (Term i, Environment (Loc i), Store Identity i)
type Trace i f = f (TraceEntry i)

type TracingInterpreter i f = Writer (Trace i f) ': Interpreter Identity i

type TraceInterpreter i = Writer (Trace i []) ': Interpreter Identity i
type ReachableStateInterpreter i = Writer (Trace i Set.Set) ': Interpreter Identity i


-- Tracing and reachable state analyses

evalTrace :: forall i. AbstractValue i (Eff (TraceInterpreter i)) => Term i -> (Either String (Val i, Trace i []), Store Identity i)
evalTrace = run @(TraceInterpreter i) . runTrace

runTrace :: (TraceInterpreter i :<: fs, AbstractValue i (Eff fs)) => Term i -> Eff fs (Val i)
runTrace = fix (evTell [] (ev (undefined :: proxy Identity)))

evalReach :: forall i. (Ord i, AbstractValue i (Eff (ReachableStateInterpreter i))) => Term i -> (Either String (Val i, Trace i Set.Set), Store Identity i)
evalReach = run @(ReachableStateInterpreter i) . runReach

runReach :: (Ord i, ReachableStateInterpreter i :<: fs, AbstractValue i (Eff fs)) => Term i -> Eff fs (Val i)
runReach = fix (evTell Set.empty (ev (undefined :: proxy Identity)))

evTell :: forall i f fs . (TracingInterpreter i f :<: fs, IsList (Trace i f), Item (Trace i f) ~ TraceEntry i)
       => f ()
       -> ((Term i -> Eff fs (Val i)) -> Term i -> Eff fs (Val i))
       -> (Term i -> Eff fs (Val i))
       -> Term i
       -> Eff fs (Val i)
evTell _ ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [(e, env, store)] :: Trace i f)
  ev0 ev e
