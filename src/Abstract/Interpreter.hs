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
import Data.Functor.Foldable
import qualified Data.Map as Map


type Interpreter l a = '[Failure, State (Store l a), Reader (Environment (l a))]


-- Evaluation

eval :: forall l i . (Monoid (Store l i), AbstractStore l, Context l i (Interpreter l i), AbstractNumber i (Eff (Interpreter l i))) => Term i -> (Either String (Value l i), Store l i)
eval = run @(Interpreter l i) . runEval

runEval :: (AbstractStore l, Context l i fs, AbstractNumber i (Eff fs), Interpreter l i :<: fs) => Term i -> Eff fs (Value l i)
runEval = fix ev

ev :: forall l i fs
   .  (AbstractStore l, Context l i fs, AbstractNumber i (Eff fs), Interpreter l i :<: fs)
   => (Term i -> Eff fs (Value l i))
   -> Term i
   -> Eff fs (Value l i)
ev ev term = case unfix term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    I <$> find ((p :: Environment (l i)) Map.! x)
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
    I v <- local (const (Map.insert f a (p :: Environment (l i)))) (ev e)
    ext a v
    return (I v)
  Lam x e0 -> do
    p <- ask
    return (Closure x e0 (p :: Environment (l i)))
  App e0 e1 -> do
    Closure x e2 p <- ev e0
    I v1 <- ev e1
    a <- alloc x
    ext a v1
    local (const (Map.insert x a p)) (ev e2)
