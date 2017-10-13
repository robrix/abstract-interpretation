{-# LANGUAGE ConstraintKinds, FunctionalDependencies, AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Value where

import Abstract.Environment
import Abstract.Primitive
import Abstract.Term
import Abstract.Set
import Abstract.Store
import Abstract.Type

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



class AbstractValue l v | v -> l where
  literal :: Prim -> v
  valueRoots :: v -> Set (Address l v)

instance (FreeVariables1 syntax, Functor syntax, Ord l) => AbstractValue l (Value syntax l) where
  valueRoots (I _) = mempty
  valueRoots (Closure name body env) = envRoots env (delete name (freeVariables body))

  literal = I

instance AbstractValue Monovariant Type where
  valueRoots _ = mempty

  literal (PInt _)  = Int
  literal (PBool _) = Bool
