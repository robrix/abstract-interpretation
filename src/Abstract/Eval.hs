{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abstract.Eval where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Fail
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Failure
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Writer
import Data.Function (fix)
import Data.Functor.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (fail)

type Environment i = Map.Map String (Loc (Val i))
data Val i = I i | L (Term i, Environment i)
  deriving (Eq, Ord, Show)

-- Evaluation

eval :: forall i . AbstractValue i (Eff (Interpreter i)) => Term i -> (Either String (Val i), Store (Val i))
eval = run @(Interpreter i) . runEval

runEval :: (Interpreter i :<: fs, AbstractValue i (Eff fs)) => Term i -> Eff fs (Val i)
runEval = fix ev

ev :: (AbstractValue i (Eff fs), Interpreter i :<: fs)
   => (Term i -> Eff fs (Val i))
   -> Term i
   -> Eff fs (Val i)
ev ev term = case unfix term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    find (p Map.! x)
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
    let p' = Map.insert f a p
    v <- local (const p') (ev e)
    ext a v
    return v
  Lam x e0 -> do
    p <- ask
    return (L (makeLam x e0, p))
  App e0 e1 -> do
    (L (Fix (Lam x e2), p)) <- ev e0
    v1 <- ev e1
    a <- alloc x
    ext a v1
    local (const (Map.insert x a p)) (ev e2)


-- Dead code analysis

evalDead :: forall i. (Ord i, AbstractValue i (Eff (DeadCodeInterpreter i))) => Term i -> (Either String (Val i, Set.Set (Term i)), Store (Val i))
evalDead = run @(DeadCodeInterpreter i) . runDead

runDead :: (Ord i, DeadCodeInterpreter i :<: fs, AbstractValue i (Eff fs)) => Term i -> Eff fs (Val i)
runDead e0 = do
  put (subexps e0)
  fix (evDead ev) e0

evDead :: (Ord i, DeadCodeInterpreter i :<: fs)
       => ((Term i -> Eff fs (Val i)) -> Term i -> Eff fs (Val i))
       -> (Term i -> Eff fs (Val i))
       -> Term i
       -> Eff fs (Val i)
evDead ev0 ev e = do
  modify (Set.delete e)
  ev0 ev e


instance (MonadFail m, AbstractValue i m) => AbstractValue (Val i) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"

type Interpreter i = '[Failure, State (Store (Val i)), Reader (Environment i)]
type Trace i f = f (TraceEntry i)
type TraceEntry i = (Term i, Environment i, Store (Val i))
type TracingInterpreter i f = Writer (Trace i f) ': Interpreter i
type TraceInterpreter i = Writer (Trace i []) ': Interpreter i
type ReachableStateInterpreter i = Writer (Trace i Set.Set) ': Interpreter i
type DeadCodeInterpreter i = State (Set.Set (Term i)) ': Interpreter i


-- Instances

instance (Real i, AbstractValue i (Eff (Interpreter i))) => Real (Term i) where
  toRational term = case fst (eval term) of
    Right (I a) -> toRational a
    Right _ -> error "toRational applied to non-numeric Term"
    Left s -> error s

instance (Enum i, Num i, AbstractValue i (Eff (Interpreter i))) => Enum (Term i) where
  toEnum = fromIntegral
  fromEnum term = case fst (eval term) of
    Right (I a) -> fromEnum a
    Right _ -> error "fromEnum applied to non-numeric Term"
    Left s -> error s

instance (Integral i, AbstractValue i (Eff (Interpreter i))) => Integral (Term i) where
  a `quot` b = Fix (Op2 Quotient a b)
  a `rem` b = Fix (Op2 Remainder a b)
  a `quotRem` b = (a `quot` b, a `rem` b)

  toInteger term = case fst (eval term) of
    Right (I a) -> toInteger a
    Right _ -> error "toInteger applied to non-numeric Term"
    Left s -> error s
