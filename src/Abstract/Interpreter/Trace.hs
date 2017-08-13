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
import qualified Data.Set as Set
import GHC.Exts (IsList(..))

type TraceEntry f i = (Term i, Environment (Loc i), Store f i)
type Trace f i g = g (TraceEntry f i)

type TracingInterpreter f i g = Writer (Trace f i g) ': Interpreter f i

type TraceInterpreter f i = TracingInterpreter f i []
type ReachableStateInterpreter f i = TracingInterpreter f i Set.Set


-- Tracing and reachable state analyses

evalTrace :: forall f i. AbstractValue i (Eff (TraceInterpreter f i)) => AbstractStore f (TraceInterpreter f i) i -> Term i -> (Either String (Val i, Trace f i []), Store f i)
evalTrace store = run @(TraceInterpreter f i) . runTrace store

runTrace :: (TraceInterpreter f i :<: fs, AbstractValue i (Eff fs)) => AbstractStore f fs i -> Term i -> Eff fs (Val i)
runTrace store = fix (evTell store [] (ev store))

evalReach :: forall f i. (Ord i, Ord (f i), AbstractValue i (Eff (ReachableStateInterpreter f i))) => AbstractStore f (ReachableStateInterpreter f i) i -> Term i -> (Either String (Val i, Trace f i Set.Set), Store f i)
evalReach store = run @(ReachableStateInterpreter f i) . runReach store

runReach :: (Ord i, Ord (f i), ReachableStateInterpreter f i :<: fs, AbstractValue i (Eff fs)) => AbstractStore f fs i -> Term i -> Eff fs (Val i)
runReach store = fix (evTell store Set.empty (ev store))

evTell :: forall f i g fs . (TracingInterpreter f i g :<: fs, IsList (Trace f i g), Item (Trace f i g) ~ TraceEntry f i)
       => AbstractStore f fs i
       -> g ()
       -> ((Term i -> Eff fs (Val i)) -> Term i -> Eff fs (Val i))
       -> (Term i -> Eff fs (Val i))
       -> Term i
       -> Eff fs (Val i)
evTell _ _ ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [(e, env, store)] :: Trace f i g)
  ev0 ev e
