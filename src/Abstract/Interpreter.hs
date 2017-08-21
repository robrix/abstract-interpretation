{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter where

import Abstract.Number
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


type Interpreter l t a = '[Failure, State (AddressStore l (Value l t a)), Reader (Environment (l (Value l t a)))]

type EvalResult l a = (Either String (Value l (Term a) a), AddressStore l (Value l (Term a) a))

type Eval t l fs a = t -> Eff fs (Value l t a)


-- Evaluation

eval :: forall l a . (Monoid (AddressStore l (Value l (Term a) a)), Address l, Context l (Value l (Term a) a) (Interpreter l (Term a) a), AbstractNumber a (Eff (Interpreter l (Term a) a))) => Term a -> EvalResult l a
eval = run @(Interpreter l (Term a) a) . runEval

runEval :: (Address l, Context l (Value l (Term a) a) fs, AbstractNumber a (Eff fs), Interpreter l (Term a) a :<: fs) => Eval (Term a) l fs a
runEval = fix ev

ev :: forall l a fs
   .  (Address l, Context l (Value l (Term a) a) fs, AbstractNumber a (Eff fs), Interpreter l (Term a) a :<: fs)
   => Eval (Term a) l fs a
   -> Eval (Term a) l fs a
ev ev term = case out term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    maybe (fail ("free variable: " ++ x)) deref (envLookup x (p :: Environment (l (Value l (Term a) a))))
  If0 c t e -> do
    v <- ev c
    z <- isZero v
    ev (if z then t else e)
  Op1 o a -> do
    va <- ev a
    delta1 o va
  Op2 o a b -> do
    va <- ev a
    vb <- ev b
    delta2 o va vb
  Rec f e -> do
    p <- ask
    a <- alloc f
    v <- local (const (envInsert f a (p :: Environment (l (Value l (Term a) a))))) (ev e)
    assign a v
    return v
  Lam x e0 -> do
    p <- ask
    return (Closure x e0 p)
  App e0 e1 -> do
    closure <- ev e0
    case closure of
      Closure x e2 p -> do
        v1 <- ev e1
        a <- alloc x
        assign a v1
        local (const (envInsert x a p)) (ev e2)
      _ -> fail "non-closure operator"
