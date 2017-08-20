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


type Interpreter l a = '[Failure, State (AddressStore l (Value l a)), Reader (Environment (l (Value l a)))]

type EvalResult l a = (Either String (Value l a), AddressStore l (Value l a))

type Eval l fs a = Term a -> Eff fs (Value l a)

type Unfixed a = a -> a


-- Evaluation

eval :: forall l a . (Monoid (AddressStore l (Value l a)), Address l, Context l (Value l a) (Interpreter l a), AbstractNumber a (Eff (Interpreter l a))) => Term a -> EvalResult l a
eval = run @(Interpreter l a) . runEval

runEval :: (Address l, Context l (Value l a) fs, AbstractNumber a (Eff fs), Interpreter l a :<: fs) => Eval l fs a
runEval = fix ev

ev :: forall l a fs
   .  (Address l, Context l (Value l a) fs, AbstractNumber a (Eff fs), Interpreter l a :<: fs)
   => Unfixed (Eval l fs a)
ev ev term = case out term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    maybe (fail ("free variable: " ++ x)) deref (envLookup x (p :: Environment (l (Value l a))))
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
    v <- local (const (envInsert f a (p :: Environment (l (Value l a))))) (ev e)
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
