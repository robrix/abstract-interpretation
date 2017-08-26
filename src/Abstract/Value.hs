{-# LANGUAGE AllowAmbiguousTypes, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
module Abstract.Value where

import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Type
import Control.Monad hiding (fail)
import Control.Monad.Effect
import Control.Monad.Effect.Failure
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Semigroup
import Data.Text.Prettyprint.Doc
import Prelude hiding (fail)
import Text.Show

newtype Environment a = Environment { unEnvironment :: Map.Map Name a }
  deriving (Eq, Eq1, Foldable, Functor, Monoid, Ord, Ord1, Show, Show1, Traversable)

envLookup :: Name -> Environment a -> Maybe a
envLookup = (. unEnvironment) . Map.lookup

envInsert :: Name -> a -> Environment a -> Environment a
envInsert name value (Environment m) = Environment (Map.insert name value m)


data Value l t a
  = I a
  | Closure Name t (Environment (Key l (Value l t a)))
  deriving (Foldable, Functor, Traversable)


class AbstractValue l v t a where
  lambda :: (Address l, Context l (Eff fs), Reader (Environment (Key l v)) :< fs, State (Store l v) :< fs, Failure :< fs) => (t a -> Eff fs v) -> Name -> Type -> t a -> Eff fs v
  app :: (Address l, Context l (Eff fs), Reader (Environment (Key l v)) :< fs, State (Store l v) :< fs, Failure :< fs) => (t a -> Eff fs v) -> v -> v -> Eff fs v

  prim' :: a -> Eff fs v

instance AbstractValue l (Value l (t a) a) t a where
  lambda _ name _ body = do
    env <- ask
    return (Closure name body (env :: Environment (Key l (Value l (t a) a))))

  app ev (Closure x e2 p) v1 = do
    a <- alloc x
    assign a v1
    local (const (envInsert x a p)) (ev e2)
  app _ _ _ = fail "non-closure operator"

  prim' = return . I

instance AbstractValue l Type t Prim where
  lambda ev name inTy body = do
    a <- alloc name
    assign a inTy
    outTy <- local (envInsert name (a :: Key l Type)) (ev body)
    return (inTy :-> outTy)

  app _ (inTy :-> outTy) argTy = do
    unless (inTy == argTy) (fail ("expected " ++ show inTy ++ " but got " ++ show argTy))
    return outTy
  app _ op _ = fail $ "non-function operator: " ++ show op

  prim' (PInt _)  = return Int
  prim' (PBool _) = return Bool


instance Address l => Eq2 (Value l) where
  liftEq2 eqT eqA = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> a `eqA` b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> s1 == s2 && eqT t1 t2 && liftEq (liftEq go) e1 e2
            _ -> False

instance (Address l, Eq t) => Eq1 (Value l t) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq t, Address l) => Eq (Value l t a) where
  (==) = eq1

instance Address l => Ord2 (Value l) where
  liftCompare2 compareT compareA = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> compareA a b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> compare s1 s2 <> compareT t1 t2 <> liftCompare (liftCompare go) e1 e2
            (I _, _) -> LT
            _ -> GT

instance (Address l, Ord t) => Ord1 (Value l t) where
  liftCompare = liftCompare2 compare

instance (Ord a, Ord t, Address l) => Ord (Value l t a) where
  compare = compare1


instance Address l => Show2 (Value l) where
  liftShowsPrec2 spT _ spA _ = go
    where go d v = case v of
            I a -> showsUnaryWith spA "I" d a
            Closure s t e -> showsConstructor "Closure" d [flip showsPrec s, flip spT t, flip (liftShowsPrec (liftShowsPrec go (showListWith (go 0))) (liftShowList go (showListWith (go 0)))) e]

instance (Address l, Show t) => Show1 (Value l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show t, Address l) => Show (Value l t a) where
  showsPrec = showsPrec1


instance Pretty1 Environment where
  liftPretty p pl = list . map (liftPretty p pl) . Map.toList . unEnvironment

instance Pretty a => Pretty (Environment a) where
  pretty = liftPretty pretty prettyList

instance Pretty l => Pretty2 (Value l) where
  liftPretty2 pT _ pA _ = go
    where go (I a) = pA a
          go (Closure n t e) = pretty "Closure" <+> pretty n <+> dot <+> pT t <> line
                                 <> liftPretty (liftPretty go (list . map go)) (list . map (liftPretty go (list . map go))) e

instance (Pretty l, Pretty t) => Pretty1 (Value l t) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty l, Pretty t, Pretty a) => Pretty (Value l t a) where
  pretty = liftPretty pretty prettyList


instance (Failure :< fs, PrimitiveOperations a fs) => PrimitiveOperations (Value l t a) fs where
  delta1 o   (I a) = fmap I (delta1 o a)
  delta1 Not _     = nonBoolean
  delta1 _   _     = nonNumeric

  delta2 o   (I a)     (I b)     = fmap I (delta2 o a b)
  delta2 And _         _         = nonBoolean
  delta2 Or  _         _         = nonBoolean
  delta2 XOr _         _         = nonBoolean
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
