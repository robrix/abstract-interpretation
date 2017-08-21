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

type TracingInterpreter l a g = Writer (g (Configuration l (Term a) (Value l (Term a) a))) ': Interpreter l (Term a) a

type TraceInterpreter l a = TracingInterpreter l a []
type ReachableStateInterpreter l a = TracingInterpreter l a Set.Set

type TraceResult l a f = (Either String (Value l (Term a) a, f (Configuration l (Term a) (Value l (Term a) a))), Store l (Value l (Term a) a))


-- Tracing and reachable state analyses

evalTrace :: forall a l. (Monoid (Store l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) (TraceInterpreter l a), AbstractNumber a (Eff (TraceInterpreter l a))) => Term a -> TraceResult l a []
evalTrace = run @(TraceInterpreter l a) . runTrace

runTrace :: (TraceInterpreter l a :<: fs, Address l, Context l (Value l (Term a) a) fs, AbstractNumber a (Eff fs)) => Eval (Term a) fs (Value l (Term a) a)
runTrace = fix (evTell [] ev)

evalReach :: forall a l. (Monoid (Store l (Value l (Term a) a)), Ord a, Ord (l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) (ReachableStateInterpreter l a), AbstractNumber a (Eff (ReachableStateInterpreter l a))) => Term a -> TraceResult l a Set.Set
evalReach = run @(ReachableStateInterpreter l a) . runReach

runReach :: (Ord a, Ord (l (Value l (Term a) a)), ReachableStateInterpreter l a :<: fs, Address l, Context l (Value l (Term a) a) fs, AbstractNumber a (Eff fs)) => Eval (Term a) fs (Value l (Term a) a)
runReach = fix (evTell Set.empty ev)

evTell :: forall l a g fs . (TracingInterpreter l a g :<: fs, IsList (g (Configuration l (Term a) (Value l (Term a) a))), Item (g (Configuration l (Term a) (Value l (Term a) a))) ~ Configuration l (Term a) (Value l (Term a) a))
       => g ()
       -> (Eval (Term a) fs (Value l (Term a) a) -> Eval (Term a) fs (Value l (Term a) a))
       -> Eval (Term a) fs (Value l (Term a) a)
       -> Eval (Term a) fs (Value l (Term a) a)
evTell _ ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [Configuration e env store] :: g (Configuration l (Term a) (Value l (Term a) a)))
  ev0 ev e
