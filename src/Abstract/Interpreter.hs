{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter where

import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Failure
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Semigroup
import Prelude hiding (fail)


type Interpreter l v = '[Failure, State (Store l v), Reader (Environment l v)]

type MonadInterpreter l v m = (MonadEnv l v m, MonadStore l v m, MonadFail m)

type EvalResult l v = Final (Interpreter l v) v

type Eval t m = t -> m


-- Evaluation

eval :: forall l v . (MonadAddress l (Eff (Interpreter l v)), MonadValue l v (Term Prim) (Eff (Interpreter l v)), MonadPrim v (Eff (Interpreter l v)), Semigroup (Cell l v)) => Term Prim -> EvalResult l v
eval = run @(Interpreter l v) . fix (ev @l)

ev :: forall l v m
   .  (MonadAddress l m, MonadValue l v (Term Prim) m, MonadInterpreter l v m, MonadPrim v m, Semigroup (Cell l v))
   => Eval (Term Prim) (m v)
   -> Eval (Term Prim) (m v)
ev ev term = case out term of
  Var x -> do
    p <- askEnv
    maybe (fail ("free variable: " ++ x)) deref (envLookup x (p :: Environment l v))
  Prim n -> return (literal n)
  Op1 o a -> do
    va <- ev a
    delta1 o va
  Op2 o a b -> do
    va <- ev a
    vb <- ev b
    delta2 o va vb
  App e0 e1 -> do
    closure <- ev e0
    v1 <- ev e1
    app @l ev closure v1
  Lam x ty e0 -> lambda @l ev x ty e0
  Rec x ty e0 -> rec @l ev x ty e0
  If c t e -> do
    v <- ev c
    c' <- truthy v
    ev (if c' then t else e)
