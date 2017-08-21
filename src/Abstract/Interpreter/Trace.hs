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

type TracingInterpreter l t v g = Writer (g (Configuration l t v)) ': Interpreter l v

type TraceInterpreter l t v = TracingInterpreter l t v []
type ReachableStateInterpreter l t v = TracingInterpreter l t v Set.Set

type TraceResult l t v f = (Either String (v, f (Configuration l t v)), Store l v)


-- Tracing and reachable state analyses

evalTrace :: forall a l. (Address l, Context l (Value l (Term a) a) (TraceInterpreter l (Term a) (Value l (Term a) a)), AbstractNumber a (Eff (TraceInterpreter l (Term a) (Value l (Term a) a)))) => Term a -> TraceResult l (Term a) (Value l (Term a) a) []
evalTrace = run @(TraceInterpreter l (Term a) (Value l (Term a) a)) . runTrace (undefined :: proxy l) ev

runTrace :: (TraceInterpreter l t v :<: fs, Address l) => proxy l -> (Eval t fs v -> Eval t fs v) -> Eval t fs v
runTrace proxy ev = fix (evTell proxy [] ev)

evalReach :: forall a l. (Ord a, Address l, Context l (Value l (Term a) a) (ReachableStateInterpreter l (Term a) (Value l (Term a) a)), AbstractNumber a (Eff (ReachableStateInterpreter l (Term a) (Value l (Term a) a)))) => Term a -> TraceResult l (Term a) (Value l (Term a) a) Set.Set
evalReach = run @(ReachableStateInterpreter l (Term a) (Value l (Term a) a)) . runReach (undefined :: proxy l) ev

runReach :: (Ord t, Ord v, ReachableStateInterpreter l t v :<: fs, Address l) => proxy l -> (Eval t fs v -> Eval t fs v) -> Eval t fs v
runReach proxy ev = fix (evTell proxy Set.empty ev)

evTell :: forall l t v g fs proxy . (TracingInterpreter l t v g :<: fs, IsList (g (Configuration l t v)), Item (g (Configuration l t v)) ~ Configuration l t v)
       => proxy l
       -> g ()
       -> (Eval t fs v -> Eval t fs v)
       -> Eval t fs v
       -> Eval t fs v
evTell _ _ ev0 ev e = do
  env <- ask
  store <- get
  tell (fromList [Configuration e env store] :: g (Configuration l t v))
  ev0 ev e
