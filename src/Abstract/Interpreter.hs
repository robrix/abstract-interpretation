{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
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
import Prelude hiding (fail)


type Interpreter l v = '[Failure, State (Store l v), Reader (Environment (Key l v))]

type EvalResult l v = Final (Interpreter l v) v

type Eval t m = t -> m


-- Evaluation

eval :: forall l v a . (Address l, Context l (Eff (Interpreter l v)), AbstractValue l v Term a, PrimitiveOperations v (Interpreter l v)) => Term a -> EvalResult l v
eval = run @(Interpreter l v) . runEval @l

runEval :: forall l v a fs . (Address l, Context l (Eff fs), AbstractValue l v Term a, PrimitiveOperations v fs, Interpreter l v :<: fs) => Eval (Term a) (Eff fs v)
runEval = fix (ev @l)

ev :: forall l v a fs
   .  (Address l, Context l (Eff fs), AbstractValue l v Term a, PrimitiveOperations v fs, Interpreter l v :<: fs)
   => Eval (Term a) (Eff fs v)
   -> Eval (Term a) (Eff fs v)
ev ev term = case out term of
  Var x -> do
    p <- ask
    maybe (fail ("free variable: " ++ x)) deref (envLookup x (p :: Environment (Key l v)))
  Prim n -> prim' @l @v @Term n
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
  Rec f _ e -> do
    a <- alloc f
    v <- local (envInsert f (a :: Key l v)) (ev e)
    assign a v
    return v
  If c t e -> do
    v <- ev c
    c' <- truthy v
    ev (if c' then t else e)
