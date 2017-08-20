{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Trace where

import Abstract.Configuration
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

type TracingInterpreter l i g = Writer (g (Configuration l i)) ': Interpreter l i

type TraceInterpreter l i = TracingInterpreter l i []
type ReachableStateInterpreter l i = TracingInterpreter l i Set.Set

type TraceResult l a f = (Either String (Value l a, f (Configuration l a)), AddressStore l (Value l a))


-- Tracing and reachable state analyses

evalTrace :: forall i l. (Monoid (AddressStore l (Value l i)), Address l, Context l (Value l i) (TraceInterpreter l i), AbstractNumber i (Eff (TraceInterpreter l i))) => Term i -> TraceResult l i []
evalTrace = run @(TraceInterpreter l i) . runTrace

runTrace :: (TraceInterpreter l i :<: fs, Address l, Context l (Value l i) fs, AbstractNumber i (Eff fs)) => Term i -> Eff fs (Value l i)
runTrace = fix (evTell [] ev)

evalReach :: forall i l. (Monoid (AddressStore l (Value l i)), Ord i, Ord (l (Value l i)), Ord (AddressStore l (Value l i)), Address l, Context l (Value l i) (ReachableStateInterpreter l i), AbstractNumber i (Eff (ReachableStateInterpreter l i))) => Term i -> TraceResult l i Set.Set
evalReach = run @(ReachableStateInterpreter l i) . runReach

runReach :: (Ord i, Ord (l (Value l i)), Ord (AddressStore l (Value l i)), ReachableStateInterpreter l i :<: fs, Address l, Context l (Value l i) fs, AbstractNumber i (Eff fs)) => Term i -> Eff fs (Value l i)
runReach = fix (evTell Set.empty ev)

evTell :: forall l i g fs . (TracingInterpreter l i g :<: fs, IsList (g (Configuration l i)), Item (g (Configuration l i)) ~ Configuration l i)
       => g ()
       -> ((Term i -> Eff fs (Value l i)) -> Term i -> Eff fs (Value l i))
       -> (Term i -> Eff fs (Value l i))
       -> Term i
       -> Eff fs (Value l i)
evTell _ ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [Configuration e env store] :: g (Configuration l i))
  ev0 ev e
