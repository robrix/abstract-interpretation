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

newtype Environment l a = Environment { unEnvironment :: Map.Map Name (Address l a) }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

envLookup :: Name -> Environment l a -> Maybe (Address l a)
envLookup = (. unEnvironment) . Map.lookup

envInsert :: Name -> Address l a -> Environment l a -> Environment l a
envInsert name value (Environment m) = Environment (Map.insert name value m)


data Value l
  = I Prim
  | Closure Name (Term Prim) (Environment l (Value l))


class Monad m => MonadEnv l a m where
  askEnv :: m (Environment l a)
  localEnv :: (Environment l a -> Environment l a) -> m b -> m b

instance Reader (Environment l a) :< fs => MonadEnv l a (Eff fs) where
  askEnv = ask
  localEnv = local


class Monad m => MonadValue l v t a m where
  lambda :: (t a -> m v) -> Name -> Type -> t a -> m v
  app :: (t a -> m v) -> v -> v -> m v

  literal :: a -> m v

instance (MonadAddress l m, MonadStore l (Value l) m, MonadEnv l (Value l) m, MonadFail m, Semigroup (Cell l (Value l))) => MonadValue l (Value l) Term Prim m where
  lambda _ name _ body = do
    env <- askEnv
    return (Closure name body (env :: Environment l (Value l)))

  app ev (Closure x e2 p) v1 = do
    a <- alloc x
    assign a v1
    localEnv (const (envInsert x a p)) (ev e2)
  app _ _ _ = fail "non-closure operator"

  literal = return . I

instance (MonadAddress l m, MonadStore l Type m, MonadEnv l Type m, MonadFail m, Semigroup (Cell l Type)) => MonadValue l Type t Prim m where
  lambda ev name inTy body = do
    a <- alloc name
    assign a inTy
    outTy <- localEnv (envInsert name (a :: Address l Type)) (ev body)
    return (inTy :-> outTy)

  app _ (inTy :-> outTy) argTy = do
    unless (inTy == argTy) (fail ("expected " ++ show inTy ++ " but got " ++ show argTy))
    return outTy
  app _ op _ = fail $ "non-function operator: " ++ show op

  literal (PInt _)  = return Int
  literal (PBool _) = return Bool


instance Eq2 Environment where
  liftEq2 eqL eqA (Environment m1) (Environment m2) = liftEq (liftEq2 eqL eqA) m1 m2

instance Eq l => Eq1 (Environment l) where
  liftEq = liftEq2 (==)

instance Eq1 Value where
  liftEq eqL = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> a == b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> s1 == s2 && t1 == t2 && liftEq2 eqL go e1 e2
            _ -> False

instance Eq l => Eq (Value l) where
  (==) = eq1

instance Ord2 Environment where
  liftCompare2 compareL compareA (Environment m1) (Environment m2) = liftCompare (liftCompare2 compareL compareA) m1 m2

instance Ord l => Ord1 (Environment l) where
  liftCompare = liftCompare2 compare

instance Ord1 Value where
  liftCompare compareL = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> compare a b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> compare s1 s2 <> compare t1 t2 <> liftCompare2 compareL go e1 e2
            (I _, _) -> LT
            _ -> GT

instance Ord l => Ord (Value l) where
  compare = compare1


instance Show2 Environment where
  liftShowsPrec2 spL slL spA slA d (Environment m) = showsConstructor "Environment" d [ flip (liftShowsPrec showsAddress (showListWith (showsAddress 0))) m ]
    where showsAddress = liftShowsPrec2 spL slL spA slA

instance Show l => Show1 (Environment l) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show1 Value where
  liftShowsPrec spL slL = go
    where go d v = case v of
            I a -> showsUnaryWith showsPrec "I" d a
            Closure s t e -> showsConstructor "Closure" d [flip showsPrec s, flip showsPrec t, flip (liftShowsPrec2 spL slL go (showListWith (go 0))) e]

instance Show l => Show (Value l) where
  showsPrec = showsPrec1


instance Pretty2 Environment where
  liftPretty2 pL plL pA plA = list . map (liftPretty prettyAddress (list . map prettyAddress)) . Map.toList . unEnvironment
    where prettyAddress = liftPretty2 pL plL pA plA

instance Pretty l => Pretty1 (Environment l) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty l, Pretty a) => Pretty (Environment l a) where
  pretty = liftPretty pretty prettyList

instance Pretty1 Value where
  liftPretty pL plL = go
    where go (I a) = pretty a
          go (Closure n t e) = pretty "Closure" <+> pretty n <+> dot <+> pretty t <> line
                                 <> liftPretty2 pL plL go (list . map go) e

instance Pretty l => Pretty (Value l) where
  pretty = liftPretty pretty prettyList


instance MonadFail m => MonadPrim (Value l) m where
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
