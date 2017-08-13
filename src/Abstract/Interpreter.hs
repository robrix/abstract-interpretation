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

eval :: forall f i . (AbstractStore f, AbstractValue i (Eff (Interpreter f i))) => Term i -> (Either String (Val i), Store f i)
eval = run @(Interpreter f i) . runEval (undefined :: proxy f)

runEval :: (AbstractStore f, AbstractValue i (Eff fs), Interpreter f i :<: fs) => proxy f -> Term i -> Eff fs (Val i)
runEval proxy = fix (ev proxy)

ev :: forall f i fs proxy
   .  (AbstractStore f, AbstractValue i (Eff fs), Interpreter f i :<: fs)
   => proxy f
   -> (Term i -> Eff fs (Val i))
   -> Term i
   -> Eff fs (Val i)
ev proxy ev term = case unfix term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    I <$> find proxy (p Map.! x)
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
    a <- alloc proxy f
    I v <- local (const (Map.insert f a p)) (ev e)
    ext proxy a v
    return (I v)
  Lam x e0 -> do
    p <- ask
    return (L (makeLam x e0, p))
  App e0 e1 -> do
    L (Fix (Lam x e2), p) <- ev e0
    I v1 <- ev e1
    a <- alloc proxy x
    ext proxy a v1
    local (const (Map.insert x a p)) (ev e2)


instance (MonadFail m, AbstractValue i m) => AbstractValue (Val i) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"
