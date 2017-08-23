{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
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


type Interpreter l v = '[Failure, State (Store l v), Reader (Environment (l v))]

type EvalResult l a = (Either String (Value l (Term a) a), Store l (Value l (Term a) a))

type Eval t fs v = t -> Eff fs v


-- Evaluation

eval :: forall l a . (Address l, Context l (Value l (Term a) a) (Interpreter l (Value l (Term a) a)), PrimitiveOperations a (Eff (Interpreter l (Value l (Term a) a)))) => Term a -> EvalResult l a
eval = run @(Interpreter l (Value l (Term a) a)) . runEval

runEval :: (Address l, Context l (Value l (Term a) a) fs, PrimitiveOperations a (Eff fs), Interpreter l (Value l (Term a) a) :<: fs) => Eval (Term a) fs (Value l (Term a) a)
runEval = fix ev

ev :: forall l a fs
   .  (Address l, Context l (Value l (Term a) a) fs, PrimitiveOperations a (Eff fs), Interpreter l (Value l (Term a) a) :<: fs)
   => Eval (Term a) fs (Value l (Term a) a)
   -> Eval (Term a) fs (Value l (Term a) a)
ev ev term = case out term of
  Prim n -> return (I n)
  Var x -> do
    p <- ask
    maybe (fail ("free variable: " ++ x)) (deref . snd) (envLookup x (p :: Environment (l (Value l (Term a) a))))
  If c t e -> do
    v <- ev c
    c' <- truthy v
    ev (if c' then t else e)
  Op1 o a -> do
    va <- ev a
    delta1 o va
  Op2 o a b -> do
    va <- ev a
    vb <- ev b
    delta2 o va vb
  Rec f ty e -> do
    p <- ask
    a <- alloc f
    v <- local (const (envInsert f ty a (p :: Environment (l (Value l (Term a) a))))) (ev e)
    assign a v
    return v
  Lam x ty e0 -> do
    p <- ask
    return (Closure x ty e0 p)
  App e0 e1 -> do
    closure <- ev e0
    case closure of
      Closure x ty e2 p -> do
        v1 <- ev e1
        a <- alloc x
        assign a v1
        local (const (envInsert x ty a p)) (ev e2)
      _ -> fail "non-closure operator"
