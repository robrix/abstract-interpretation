{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Value where

import Abstract.Environment
import Abstract.Primitive
import Abstract.Term

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Functor.Classes
import Data.Semigroup
import Prelude hiding (fail)


data Value syntax l
  = I Prim
  | Closure Name (Term syntax) (Environment l (Value syntax l))
  deriving (Eq, Ord, Show)


-- Instances

-- instance Eq2 Value where
  -- liftEq2 eql1 eql2 = go
  --   where go v1 v2 = case (v1, v2) of
  --           _ -> False

instance Eq1 syntax => Eq1 (Value syntax) where
  liftEq eqL = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> a == b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> s1 == s2 && t1 == t2 && liftEq2 eqL go e1 e2
            _ -> False

instance Ord1 syntax => Ord1 (Value syntax) where
  liftCompare compareL = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> compare a b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> compare s1 s2 <> compare t1 t2 <> liftCompare2 compareL go e1 e2
            (I _, _) -> LT
            _ -> GT

-- instance Show1 Value where
--   liftShowsPrec spL slL = go
--     where go d v = case v of
--             I a -> showsUnaryWith showsPrec "I" d a
--             Closure s t e -> showsConstructor "Closure" d [flip showsPrec s, flip showsPrec t, flip (liftShowsPrec2 spL slL go (showListWith (go 0))) e]
--
-- instance Show l => Show (Value l) where
--   showsPrec = showsPrec1

instance MonadFail m => MonadPrim (Value s l) m where
  delta1 o   (I a) = fmap I (delta1 o a)
  delta1 Not _     = nonBoolean
  delta1 _   _     = nonNumeric

  delta2 o   (I a)     (I b)     = fmap I (delta2 o a b)
  delta2 And _         _         = nonBoolean
  delta2 Or  _         _         = nonBoolean
  delta2 Eq  Closure{} Closure{} = undefinedComparison
  delta2 Eq  _         _         = disjointComparison
  delta2 Lt  Closure{} Closure{} = undefinedComparison
  delta2 Lt  _         _         = disjointComparison
  delta2 LtE Closure{} Closure{} = undefinedComparison
  delta2 LtE _         _         = disjointComparison
  delta2 Gt  Closure{} Closure{} = undefinedComparison
  delta2 Gt  _         _         = disjointComparison
  delta2 GtE Closure{} Closure{} = undefinedComparison
  delta2 GtE _         _         = disjointComparison
  delta2 _   _         _         = nonNumeric

  truthy (I a) = truthy a
  truthy _     = nonBoolean


-- class AbstractValue l v | v -> l where
--   literal :: Prim -> v
--   valueRoots :: v -> Set (Address l v)

-- class (AbstractValue l v, Monad m) => MonadValue l v t m where
--   rec :: (t -> m v) -> Name -> t -> m v
--   lambda :: (t -> m v) -> Name -> t -> m v
--   app :: (t -> m v) -> v -> v -> m v

-- instance (MonadAddress l m, MonadStore l (Value l) m, MonadEnv l (Value l) m, MonadFail m, Semigroup (Cell l (Value l))) => MonadValue l (Value l) (Term Prim) m where
--   rec ev name e0 =  do
--     a <- alloc name
--     v <- localEnv (envInsert name (a :: Address l (Value l))) (ev e0)
--     assign a v
--     return v
--
--   lambda _ name body = do
--     env <- askEnv
--     return (Closure name body (env :: Environment l (Value l)))
--
--   app ev (Closure x e2 p) v1 = do
--     a <- alloc x
--     assign a v1
--     localEnv (const (envInsert x a p)) (ev e2)
--   app _ _ _ = fail "non-closure operator"
--
-- instance Ord l => AbstractValue l (Value l) where
--   valueRoots (I _) = mempty
--   valueRoots (Closure name body env) = envRoots env (delete name (freeVariables body))
--
--   literal = I
--
-- instance (MonadStore Monovariant Type m, MonadEnv Monovariant Type m, MonadFail m, Semigroup (Cell Monovariant Type), MonadFresh m, Alternative m) => MonadValue Monovariant Type t m where
--   rec ev name e0 =  do
--     a <- alloc name
--     tvar <- fresh
--     assign a (TVar tvar)
--     v <- localEnv (envInsert name (a :: Address Monovariant Type)) (ev e0)
--     return v
--
--   lambda ev name body = do
--     a <- alloc name
--     tvar <- fresh
--     assign a (TVar tvar)
--     outTy <- localEnv (envInsert name (a :: Address Monovariant Type)) (ev body)
--     return (TVar tvar :-> outTy)
--
--   app _ opTy inTy = do
--     tvar <- fresh
--     _ :-> outTy <- opTy `unify` (inTy :-> TVar tvar)
--     return outTy
--
-- instance AbstractValue Monovariant Type where
--   valueRoots _ = mempty
--
--   literal (PInt _)  = Int
--   literal (PBool _) = Bool
--
