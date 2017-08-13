{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Fail
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Failure
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Functor.Foldable
import qualified Data.Map as Map
import Prelude hiding (fail)


type Environment = Map.Map String

data Val i = I i | L (Term i, Environment (Loc i))
  deriving (Eq, Ord, Show)


type Interpreter f a = '[Failure, State (Store f a), Reader (Environment (Loc a))]


-- Evaluation

eval :: forall f i . (AbstractValue i (Eff (Interpreter f i))) => AbstractStore f (Interpreter f i) i -> Term i -> (Either String (Val i), Store f i)
eval store = run @(Interpreter f i) . runEval store

runEval :: (AbstractValue i (Eff fs), Interpreter f i :<: fs) => AbstractStore f fs i -> Term i -> Eff fs (Val i)
runEval store = fix (ev store)

ev :: (AbstractValue i (Eff fs), Interpreter f i :<: fs)
   => AbstractStore f fs i
   -> (Term i -> Eff fs (Val i))
   -> Term i
   -> Eff fs (Val i)
ev store ev term = case unfix term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    I <$> find store (p Map.! x)
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
    a <- alloc store f
    I v <- local (const (Map.insert f a p)) (ev e)
    ext store a v
    return (I v)
  Lam x e0 -> do
    p <- ask
    return (L (makeLam x e0, p))
  App e0 e1 -> do
    L (Fix (Lam x e2), p) <- ev e0
    I v1 <- ev e1
    a <- alloc store x
    ext store a v1
    local (const (Map.insert x a p)) (ev e2)


instance (MonadFail m, AbstractValue i m) => AbstractValue (Val i) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"
