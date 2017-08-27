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


data Value l
  = I Prim
  | Closure Name (Term Prim) (Environment (Address l (Value l)))


class Monad m => MonadEnv a m where
  askEnv :: m (Environment a)
  localEnv :: (Environment a -> Environment a) -> m b -> m b

instance Reader (Environment a) :< fs => MonadEnv a (Eff fs) where
  askEnv = ask
  localEnv = local


class Monad m => MonadValue l v t a m where
  lambda :: (t a -> m v) -> Name -> Type -> t a -> m v
  app :: (t a -> m v) -> v -> v -> m v

  prim' :: a -> m v

instance (MonadAddress l m, MonadStore l (Value l) m, MonadEnv (Address l (Value l)) m, MonadFail m, Semigroup (Cell l (Value l))) => MonadValue l (Value l) Term Prim m where
  lambda _ name _ body = do
    env <- askEnv
    return (Closure name body (env :: Environment (Address l (Value l))))

  app ev (Closure x e2 p) v1 = do
    a <- alloc x
    assign a v1
    localEnv (const (envInsert x a p)) (ev e2)
  app _ _ _ = fail "non-closure operator"

  prim' = return . I

instance (MonadAddress l m, MonadStore l Type m, MonadEnv (Address l Type) m, MonadFail m, Semigroup (Cell l Type)) => MonadValue l Type t Prim m where
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


instance Eq1 Value where
  liftEq eqL = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> a == b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> s1 == s2 && t1 == t2 && liftEq (liftEq2 eqL go) e1 e2
            _ -> False

instance Eq l => Eq (Value l) where
  (==) = eq1

instance Ord1 Value where
  liftCompare compareL = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> compare a b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> compare s1 s2 <> compare t1 t2 <> liftCompare (liftCompare2 compareL go) e1 e2
            (I _, _) -> LT
            _ -> GT

instance Ord l => Ord (Value l) where
  compare = compare1


instance Show1 Value where
  liftShowsPrec spL slL = go
    where go d v = case v of
            I a -> showsUnaryWith showsPrec "I" d a
            Closure s t e -> showsConstructor "Closure" d [flip showsPrec s, flip showsPrec t, flip (liftShowsPrec (liftShowsPrec2 spL slL go (showListWith (go 0))) (liftShowList2 spL slL go (showListWith (go 0)))) e]

instance Show l => Show (Value l) where
  showsPrec = showsPrec1


instance Pretty1 Environment where
  liftPretty p pl = list . map (liftPretty p pl) . Map.toList . unEnvironment

instance Pretty a => Pretty (Environment a) where
  pretty = liftPretty pretty prettyList

instance Pretty1 Value where
  liftPretty pL plL = go
    where go (I a) = pretty a
          go (Closure n t e) = pretty "Closure" <+> pretty n <+> dot <+> pretty t <> line
                                 <> liftPretty (liftPretty2 pL plL go (list . map go)) (list . map (liftPretty2 pL plL go (list . map go))) e

instance Pretty l => Pretty (Value l) where
  pretty = liftPretty pretty prettyList


instance MonadFail m => MonadPrim (Value l) m where
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
