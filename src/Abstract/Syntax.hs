{-# LANGUAGE TypeApplications, DataKinds, TypeFamilies, ConstraintKinds, AllowAmbiguousTypes, DeriveAnyClass, DeriveFunctor, DeriveFoldable, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Syntax where

import Abstract.Environment
import Abstract.Eval
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Abstract.Term
import Abstract.Type
import Abstract.Value

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Effect
import Control.Monad.Fail
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Pointed
import Data.Semigroup
import Data.Union
import GHC.Generics
import Prelude hiding (fail)

-- The Syntax of our language, defined as an open Union of type constructors.
type Syntax = Union
  '[ Variable
   , Primitive
   , Lambda
   , Application
   , Rec
   , Unary
   , Binary
   , If
   ]



-- Variables
newtype Variable a = Variable String deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Variable where liftEq = genericLiftEq
instance Ord1 Variable where liftCompare = genericLiftCompare

instance (Monad m, MonadFail m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m) => Eval (Value s l) m s Variable where
  evaluate _ (Variable x) = do
    env <- askEnv
    maybe (fail ("free variable: " ++ x)) deref (envLookup x (env :: Environment l (Value s l)))

instance (Alternative m, MonadFail m, MonadStore Monovariant Type m, MonadEnv Monovariant Type m) => Eval Type m s Variable where
  evaluate _ (Variable x) = do
    env <- askEnv
    maybe (fail ("free type: " ++ x)) deref (envLookup x (env :: Environment Monovariant Type))

instance FreeVariables1 Variable where
  liftFreeVariables _ (Variable name) = point name

instance (Monad m, Eval v m s Variable) => EvalCollect l v m s Variable


-- Primitives
newtype Primitive a = Primitive Prim deriving (Eq, Ord, Show, Functor, Foldable, Generic1, FreeVariables1)
instance Show1 Primitive where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Primitive where liftEq = genericLiftEq
instance Ord1 Primitive where liftCompare = genericLiftCompare

instance Monad m => Eval (Value s l) m s Primitive where
  evaluate _ (Primitive x) = return (I x)

instance Monad m => Eval Type m s Primitive where
  evaluate _ (Primitive (PInt _)) = return Int
  evaluate _ (Primitive (PBool _)) = return Bool

instance (Monad m, Eval v m s Primitive) => EvalCollect l v m s Primitive

-- Lambdas
data Lambda a = Lambda Name a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Lambda where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Lambda where liftEq = genericLiftEq
instance Ord1 Lambda where liftCompare = genericLiftCompare

instance (Monad m, MonadEnv l (Value s l) m) => Eval (Value s l) m s Lambda where
  evaluate _ (Lambda name body) = do
    env <- askEnv
    return (Closure name body (env :: Environment l (Value s l)))

instance (MonadStore Monovariant Type m, MonadEnv Monovariant Type m, MonadFail m, Semigroup (Cell Monovariant Type), MonadFresh m, Alternative m) => Eval Type m s Lambda where
  evaluate ev (Lambda name body) = do
    a <- alloc name
    tvar <- fresh
    assign a (TVar tvar)
    outTy <- localEnv (envInsert name (a :: Address Monovariant Type)) (ev body)
    return (TVar tvar :-> outTy)

instance FreeVariables1 Lambda where
  liftFreeVariables f (Lambda name body) = delete name (f body)

instance (Monad m, Eval v m s Lambda) => EvalCollect l v m s Lambda

-- Recursive binder (e.g. letrec)
data Rec a = Rec Name a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Rec where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Rec where liftEq = genericLiftEq
instance Ord1 Rec where liftCompare = genericLiftCompare

instance (Monad m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m, Semigroup (Cell l (Value s l))) => Eval (Value s l) m s Rec where
  evaluate ev (Rec name body) = do
    a <- alloc name
    v <- localEnv (envInsert name (a :: Address l (Value s l))) (ev body)
    assign a v
    return v

instance (MonadStore Monovariant Type m, MonadEnv Monovariant Type m, MonadFail m, Semigroup (Cell Monovariant Type), MonadFresh m, Alternative m) => Eval Type m s Rec where
  evaluate ev (Rec name body) = do
    a <- alloc name
    tvar <- fresh
    assign a (TVar tvar)
    localEnv (envInsert name (a :: Address Monovariant Type)) (ev body)

instance FreeVariables1 Rec where
  liftFreeVariables f (Rec name body) = delete name (f body)

instance (Monad m, Eval v m s Rec) => EvalCollect l v m s Rec


-- Application
data Application a = Application a a deriving (Eq, Ord, Show, Functor, Foldable, Generic1, FreeVariables1)
instance Show1 Application where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Application where liftEq = genericLiftEq
instance Ord1 Application where liftCompare = genericLiftCompare

instance ( Monad m
         , MonadFail m
         , MonadAddress l m
         , MonadStore l (Value s l) m
         , MonadEnv l (Value s l) m
         , Semigroup (Cell l (Value s l))
         )
         => Eval (Value s l) m s Application where
  evaluate ev (Application e1 e2) = do
    Closure name body env <- ev e1
    value <- ev e2
    a <- alloc name
    assign a value
    localEnv (const (envInsert name a env)) (ev body)

instance ( Monad m
         , MonadFail m
         , MonadFresh m
         )
         => Eval Type m s Application where
  evaluate ev (Application e1 e2) = do
    opTy <- ev e1
    inTy <- ev e2
    tvar <- fresh
    _ :-> outTy <- opTy `unify` (inTy :-> TVar tvar)
    return outTy

instance ( Ord l
         , Monad m
         , MonadGC l (Value s l) m
         , MonadAddress l m
         , MonadStore l (Value s l) m
         , MonadEnv l (Value s l) m
         , Semigroup (Cell l (Value s l))
         , FreeVariables1 Application
         , Functor s
         , FreeVariables1 s
         )
         => EvalCollect l (Value s l) m s Application where
  evalCollect ev (Application e1 e2) = do
    env <- askEnv @l @(Value s l)
    v1@(Closure name body env') <- extraRoots (envRoots env (freeVariables e2)) (ev e1)
    v2 <- extraRoots (valueRoots @l v1) (ev e2)
    a <- alloc name
    assign a v2
    localEnv (const (envInsert name a env')) (ev body)

instance ( Ord l
         , Monad m
         , MonadFail m
         , MonadFresh m
         , MonadGC l Type m
         , MonadEnv l Type m
         , AbstractValue l Type
         , Functor s
         , FreeVariables1 s
         )
         => EvalCollect l Type m s Application where
  evalCollect ev (Application e1 e2) = do
    env <- askEnv @l @Type
    opTy <- extraRoots (envRoots env (freeVariables e2)) (ev e1)
    inTy <- extraRoots (valueRoots @l opTy) (ev e2)
    tvar <- fresh
    _ :-> outTy <- opTy `unify` (inTy :-> TVar tvar)
    return outTy


-- Unary operations
data Unary a = Unary Op1 a deriving (Eq, Ord, Show, Functor, Foldable, Generic1, FreeVariables1)
instance Show1 Unary where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Unary where liftEq = genericLiftEq
instance Ord1 Unary where liftCompare = genericLiftCompare

instance (Monad m, MonadPrim v m) => Eval v m s Unary where
  evaluate ev (Unary op e) = do
    v <- ev e
    delta1 op v

instance (Monad m, MonadPrim v m) => EvalCollect l v m s Unary

-- Binary operations
data Binary a = Binary Op2 a a deriving (Eq, Ord, Show, Functor, Foldable, Generic1, FreeVariables1)
instance Show1 Binary where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Binary where liftEq = genericLiftEq
instance Ord1 Binary where liftCompare = genericLiftCompare

instance (Monad m, MonadPrim v m) => Eval v m s Binary where
  evaluate ev (Binary op e0 e1) = do
    v1 <- ev e0
    v2 <- ev e1
    delta2 op v1 v2

instance ( Ord l
         , Monad m
         , MonadGC l v m
         , MonadEnv l v m
         , MonadPrim v m
         , Functor s
         , FreeVariables1 s
         , AbstractValue l v
         )
         => EvalCollect l v m s Binary where
  evalCollect ev (Binary op e0 e1) = do
    env <- askEnv @l @v
    v0 <- extraRoots (envRoots env (freeVariables e1)) (ev e0)
    v1 <- extraRoots (valueRoots @l v0) (ev e1)
    delta2 op v0 v1


-- If statements
data If a = If a a a deriving (Eq, Ord, Show, Functor, Foldable, Generic1, FreeVariables1)
instance Show1 If where liftShowsPrec = genericLiftShowsPrec
instance Eq1 If where liftEq = genericLiftEq
instance Ord1 If where liftCompare = genericLiftCompare

instance (Monad m, MonadPrim v m) => Eval v m s If where
  evaluate ev (If c t e) = do
    v <- ev c
    c' <- truthy v
    ev (if c' then t else e)

instance ( Ord l
         , Monad m
         , MonadGC l v m
         , MonadEnv l v m
         , MonadPrim v m
         , Functor s
         , FreeVariables1 s
         )
         => EvalCollect l v m s If where
  evalCollect ev (If c t e) = do
    env <- askEnv @l @v
    v <- extraRoots (envRoots env (freeVariables t <> freeVariables e)) (ev c)
    b <- truthy v
    ev (if b then t else e)


-- Smart constructors for various Terms.

prim :: (Primitive :< fs) => Prim -> Term (Union fs)
prim = inject . Primitive

int :: (Primitive :< fs) => Int -> Term (Union fs)
int = prim . PInt

true :: (Primitive :< fs) => Term (Union fs)
true = prim (PBool True)

false :: (Primitive :< fs) => Term (Union fs)
false = prim (PBool False)

var :: (Variable :< fs) => Name -> Term (Union fs)
var = inject . Variable

infixl 9 #
(#) :: (Application :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
(#) a b = inject (Application a b)

makeLam :: (Lambda :< fs) => Name -> Term (Union fs) -> Term (Union fs)
makeLam name body = inject (Lambda name body)

let' :: (Lambda :< fs, Application :< fs, Variable :< fs) => Name -> Term (Union fs) -> (Term (Union fs) -> Term (Union fs)) -> Term (Union fs)
let' name val body = lam name body # val
  where lam s f = makeLam s (f (var s))

makeRec :: (Rec :< fs) => Name -> Term (Union fs) -> Term (Union fs)
makeRec name body = inject (Rec name body)

mu :: (Rec :< fs, Variable :< fs) => Name -> (Term (Union fs) -> Term (Union fs)) -> Term (Union fs)
mu f b = makeRec f (b (var f))

if' :: (If :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs) -> Term (Union fs)
if' c t e = inject (If c t e)

eq :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
eq = (inject .) . Binary Eq

lt :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
lt = (inject .) . Binary Lt

lte :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
lte = (inject .) . Binary LtE

gt :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
gt = (inject .) . Binary Gt

gte :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
gte = (inject .) . Binary GtE

and' :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
and' = (inject .) . Binary And

or' :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
or' = (inject .) . Binary Or

div' :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
div' = (inject .) . Binary DividedBy

quot' :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
quot' = (inject .) . Binary Quotient

rem' :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
rem' = (inject .) . Binary Remainder

mod' :: (Binary :< fs) => Term (Union fs) -> Term (Union fs) -> Term (Union fs)
mod' = (inject .) . Binary Modulus

not' :: (Unary :< fs) => Term (Union fs) -> Term (Union fs)
not' = inject . Unary Not


instance (Binary :< fs, Unary :< fs, Primitive :< fs) => Num (Term (Union fs)) where
  fromInteger = int . fromInteger

  signum = inject     . Unary Signum
  abs    = inject     . Unary Abs
  negate = inject     . Unary Negate
  (+)    = (inject .) . Binary Plus
  (-)    = (inject .) . Binary Minus
  (*)    = (inject .) . Binary Times
