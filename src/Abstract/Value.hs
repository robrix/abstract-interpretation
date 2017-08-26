{-# LANGUAGE AllowAmbiguousTypes, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Abstract.Value where

import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Type
import Control.Monad hiding (fail)
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Fail
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
  | Closure Name t (Environment (Address l (Value l t a)))
  deriving (Foldable, Functor, Traversable)


class Monad m => MonadEnv a m where
  askEnv :: m (Environment a)
  localEnv :: (Environment a -> Environment a) -> m b -> m b

instance Reader (Environment a) :< fs => MonadEnv a (Eff fs) where
  askEnv = ask
  localEnv = local


class AbstractValue l v t a where
  lambda :: (AbstractAddress l m, MonadStore l v m, MonadEnv (Address l v) m, MonadFail m) => (t a -> m v) -> Name -> Type -> t a -> m v
  app :: (AbstractAddress l m, MonadStore l v m, MonadEnv (Address l v) m, MonadFail m) => (t a -> m v) -> v -> v -> m v

  prim' :: Monad m => a -> m v

instance AbstractValue l (Value l (t a) a) t a where
  lambda _ name _ body = do
    env <- askEnv
    return (Closure name body (env :: Environment (Address l (Value l (t a) a))))

  app ev (Closure x e2 p) v1 = do
    a <- alloc x
    assign a v1
    localEnv (const (envInsert x a p)) (ev e2)
  app _ _ _ = fail "non-closure operator"

  prim' = return . I

instance AbstractValue l Type t Prim where
  lambda ev name inTy body = do
    a <- alloc name
    assign a inTy
    outTy <- localEnv (envInsert name (a :: Address l Type)) (ev body)
    return (inTy :-> outTy)

  app _ (inTy :-> outTy) argTy = do
    unless (inTy == argTy) (fail ("expected " ++ show inTy ++ " but got " ++ show argTy))
    return outTy
  app _ op _ = fail $ "non-function operator: " ++ show op

  prim' (PInt _)  = return Int
  prim' (PBool _) = return Bool


instance Eq l => Eq2 (Value l) where
  liftEq2 eqT eqA = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> a `eqA` b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> s1 == s2 && eqT t1 t2 && liftEq (liftEq go) e1 e2
            _ -> False

instance (Eq l, Eq t) => Eq1 (Value l t) where
  liftEq = liftEq2 (==)

instance (Eq l, Eq t, Eq a) => Eq (Value l t a) where
  (==) = eq1

instance Ord l => Ord2 (Value l) where
  liftCompare2 compareT compareA = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> compareA a b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> compare s1 s2 <> compareT t1 t2 <> liftCompare (liftCompare go) e1 e2
            (I _, _) -> LT
            _ -> GT

instance (Ord l, Ord t) => Ord1 (Value l t) where
  liftCompare = liftCompare2 compare

instance (Ord l, Ord t, Ord a) => Ord (Value l t a) where
  compare = compare1


instance Show l => Show2 (Value l) where
  liftShowsPrec2 spT _ spA _ = go
    where go d v = case v of
            I a -> showsUnaryWith spA "I" d a
            Closure s t e -> showsConstructor "Closure" d [flip showsPrec s, flip spT t, flip (liftShowsPrec (liftShowsPrec go (showListWith (go 0))) (liftShowList go (showListWith (go 0)))) e]

instance (Show l, Show t) => Show1 (Value l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show l, Show t, Show a) => Show (Value l t a) where
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


instance (MonadFail m, MonadPrim a m) => MonadPrim (Value l t a) m where
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
