{-# LANGUAGE TypeApplications, ConstraintKinds, AllowAmbiguousTypes, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Abstract.Value where

import Abstract.Primitive
import Abstract.Set
import Abstract.Environment
import Abstract.Store
import Abstract.Term
import Abstract.Syntax
import Abstract.Type
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Fail
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Pointed
import Data.Semigroup
import Data.Text.Prettyprint.Doc
import Prelude hiding (fail)
import Text.Show
import Data.Proxy
import Data.Union


data Value syntax l
  = I Prim
  | Closure Name (Term Prim syntax) (Environment l (Value syntax l))
  deriving (Eq, Ord, Show)


class Monad m => Eval v m g syntax where
  eval :: (Term Prim g -> m v) -> syntax (Term Prim g) -> m v

-- Syntax Eval instance
instance (Monad m, MonadFail m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m, Semigroup (Cell l (Value s l))) => Eval (Value s l) m s Syntax where
  eval ev fs = apply (Proxy :: Proxy (Eval (Value s l) m s)) (eval ev) fs

instance (Alternative m, MonadFresh m, MonadFail m, MonadStore Monovariant Type m, MonadEnv Monovariant Type m, Semigroup (Cell Monovariant Type)) => Eval Type m s Syntax where
  eval ev fs = apply (Proxy :: Proxy (Eval Type m s)) (eval ev) fs


-- Lambda Eval instances
instance (Monad m, MonadEnv l (Value s l) m) => Eval (Value s l) m s Lambda where
  eval _ (Lambda name body) = do
    env <- askEnv
    return (Closure name body (env :: Environment l (Value s l)))

instance (MonadStore Monovariant Type m, MonadEnv Monovariant Type m, MonadFail m, Semigroup (Cell Monovariant Type), MonadFresh m, Alternative m) => Eval Type m s Lambda where
  eval ev (Lambda name body) = do
    a <- alloc name
    tvar <- fresh
    assign a (TVar tvar)
    outTy <- localEnv (envInsert name (a :: Address Monovariant Type)) (ev body)
    return (TVar tvar :-> outTy)


-- Application Eval instances
instance (Monad m, MonadFail m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m, Semigroup (Cell l (Value s l))) => Eval (Value s l) m s Application where
  eval ev (Application e1 e2) = do
    Closure name body env <- ev e1
    value <- ev e2
    a <- alloc name
    assign a value
    localEnv (const (envInsert name a env)) (ev body)

instance (Monad m, MonadFail m, MonadFresh m) => Eval Type m s Application where
  eval ev (Application e1 e2) = do
    opTy <- ev e1
    inTy <- ev e2
    tvar <- fresh
    _ :-> outTy <- opTy `unify` (inTy :-> TVar tvar)
    return outTy


-- Variable Eval instances
instance (Monad m, MonadFail m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m) => Eval (Value s l) m s Variable where
  eval _ (Variable x) = do
    env <- askEnv
    maybe (fail ("free variable: " ++ x)) deref (envLookup x (env :: Environment l (Value s l)))

instance (Alternative m, MonadFail m, MonadStore Monovariant Type m, MonadEnv Monovariant Type m) => Eval Type m s Variable where
  eval _ (Variable x) = do
    env <- askEnv
    maybe (fail ("free type: " ++ x)) deref (envLookup x (env :: Environment Monovariant Type))


-- Primitive Eval instances
instance Monad m => Eval (Value s l) m s Primitive where
  eval _ (Primitive x) = return (I x)

instance Monad m => Eval Type m s Primitive where
  eval _ (Primitive (PInt _)) = return Int
  eval _ (Primitive (PBool _)) = return Bool



class Monad m => MonadEnv l a m where
  askEnv :: m (Environment l a)
  localEnv :: (Environment l a -> Environment l a) -> m b -> m b

instance Reader (Environment l a) :< fs => MonadEnv l a (Eff fs) where
  askEnv = ask
  localEnv = local

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

-- instance (Eq l, Eq1 s) => Eq (Value s l) where
--   (==) = eq1


instance Ord1 syntax => Ord1 (Value syntax) where
  liftCompare compareL = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> compare a b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> compare s1 s2 <> compare t1 t2 <> liftCompare2 compareL go e1 e2
            (I _, _) -> LT
            _ -> GT

-- instance (Ord l, Ord1 s) => Ord (Value s l) where
--   compare = compare1


-- instance Show2 Environment where
--   liftShowsPrec2 spL slL spA slA d (Environment m) = showsConstructor "Environment" d [ flip (liftShowsPrec showsAddress (showListWith (showsAddress 0))) m ]
--     where showsAddress = liftShowsPrec2 spL slL spA slA
--
-- instance Show l => Show1 (Environment l) where
--   liftShowsPrec = liftShowsPrec2 showsPrec showList
--
-- instance Show1 Value where
--   liftShowsPrec spL slL = go
--     where go d v = case v of
--             I a -> showsUnaryWith showsPrec "I" d a
--             Closure s t e -> showsConstructor "Closure" d [flip showsPrec s, flip showsPrec t, flip (liftShowsPrec2 spL slL go (showListWith (go 0))) e]
--
-- instance Show l => Show (Value l) where
--   showsPrec = showsPrec1
--
--
-- instance Pretty2 Environment where
--   liftPretty2 pL plL pA plA = list . map (liftPretty prettyAddress (list . map prettyAddress)) . Map.toList . unEnvironment
--     where prettyAddress = liftPretty2 pL plL pA plA
--
-- instance Pretty l => Pretty1 (Environment l) where
--   liftPretty = liftPretty2 pretty prettyList
--
-- instance (Pretty l, Pretty a) => Pretty (Environment l a) where
--   pretty = liftPretty pretty prettyList
--
-- instance Pretty1 Value where
--   liftPretty pL plL = go
--     where go (I a) = pretty a
--           go (Closure n t e) = pretty "Closure" <+> pretty n <+> dot <+> pretty t <> line
--                                  <> liftPretty2 pL plL go (list . map go) e
--
-- instance Pretty l => Pretty (Value l) where
--   pretty = liftPretty pretty prettyList

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
