{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Tracing where

import Abstract.Configuration
import Abstract.Environment
import Abstract.Eval
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Abstract.Term

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Writer
import Data.Function (fix)
import Data.Functor.Classes (Ord1)
import Data.Semigroup
import GHC.Exts (IsList(..))
import qualified Data.Set as Set

type TracingInterpreter l t v g = Reader (Set (Address l v)) ': Writer (g (Configuration l t v)) ': Interpreter l v

type TraceInterpreter l t v = TracingInterpreter l t v []
type ReachableStateInterpreter l t v = TracingInterpreter l t v Set.Set

type TraceResult l t v f = Final (TracingInterpreter l t v f) v


class Monad m => MonadTrace l t v g m where
  trace :: g (Configuration l t v) -> m ()

instance (Writer (g (Configuration l t v)) :< fs) => MonadTrace l t v g (Eff fs) where
  trace = tell


-- Tracing and reachable state analyses
--
-- Examples
--    evalTrace @Precise @(Value Syntax Precise) @Syntax (makeLam "x" (var "x") # true)
--    evalReach @Precise @(Value Syntax Precise) @Syntax (makeLam "x" (var "x") # true)

evalTrace :: forall l v s
          . ( Ord v, Ord1 s, Ord1 (Cell l)
            , MonadAddress l (Eff (TraceInterpreter l (Term s) v))
            , MonadPrim v (Eff (TraceInterpreter l (Term s) v))
            , MonadGC l v (Eff (TraceInterpreter l (Term s) v))
            , Semigroup (Cell l v)
            , Eval v (Eff (TraceInterpreter l (Term s) v)) s s
            )
          => Eval' (Term s) (TraceResult l (Term s) v [])
evalTrace = run @(TraceInterpreter l (Term s) v) . fix (evTell @l @(Term s) @v @[] ev)

evalReach :: forall l v s
          . ( Ord v, Ord l, Ord1 (Cell l), Ord1 s
            , MonadAddress l (Eff (ReachableStateInterpreter l (Term s) v))
            , MonadPrim v (Eff (ReachableStateInterpreter l (Term s) v))
            , MonadGC l v (Eff (ReachableStateInterpreter l (Term s) v))
            , Semigroup (Cell l v)
            , Eval v (Eff (ReachableStateInterpreter l (Term s) v)) s s
            )
          => Eval' (Term s) (TraceResult l (Term s) v Set.Set)
evalReach = run @(ReachableStateInterpreter l (Term s) v) . fix (evTell @l @(Term s) @v @Set.Set ev)


evTell :: forall l t v g m
       . ( Ord l
         , IsList (g (Configuration l t v))
         , Item (g (Configuration l t v)) ~ Configuration l t v
         , MonadTrace l t v g m
         , MonadEnv l v m
         , MonadStore l v m
         , MonadGC l v m
         )
       => (Eval' t (m v) -> Eval' t (m v))
       -> Eval' t (m v)
       -> Eval' t (m v)
evTell ev0 ev e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  trace (fromList [Configuration e roots env store] :: g (Configuration l t v))
  ev0 ev e
