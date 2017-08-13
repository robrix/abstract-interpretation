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

type TraceInterpreter f i = Writer (Trace f i []) ': Interpreter f i
type ReachableStateInterpreter f i = Writer (Trace f i Set.Set) ': Interpreter f i


-- Tracing and reachable state analyses

evalTrace :: forall i f. (AbstractStore f, AbstractValue i (Eff (TraceInterpreter f i))) => Term i -> (Either String (Val i, Trace f i []), Store f i)
evalTrace = run @(TraceInterpreter f i) . runTrace (undefined :: proxy f)

runTrace :: (TraceInterpreter f i :<: fs, AbstractStore f, AbstractValue i (Eff fs)) => proxy f -> Term i -> Eff fs (Val i)
runTrace proxy = fix (evTell proxy [] (ev proxy))

evalReach :: forall i f. (Ord i, Ord (f i), AbstractStore f, AbstractValue i (Eff (ReachableStateInterpreter f i))) => Term i -> (Either String (Val i, Trace f i Set.Set), Store f i)
evalReach = run @(ReachableStateInterpreter f i) . runReach (undefined :: proxy f)

runReach :: (Ord i, Ord (f i), ReachableStateInterpreter f i :<: fs, AbstractStore f, AbstractValue i (Eff fs)) => proxy f -> Term i -> Eff fs (Val i)
runReach proxy = fix (evTell proxy Set.empty (ev proxy))

evTell :: forall f i g fs proxy . (TracingInterpreter f i g :<: fs, IsList (Trace f i g), Item (Trace f i g) ~ TraceEntry f i)
       => proxy f
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
