{-# LANGUAGE TypeApplications, DataKinds, TypeFamilies, ConstraintKinds, AllowAmbiguousTypes, DeriveFunctor, DeriveFoldable, DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
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
import Data.Functor.Classes.Show.Generic
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
instance Eq1 Variable where liftEq _ (Variable name1) (Variable name2) = name1 == name2
instance Ord1 Variable where liftCompare _ (Variable name1) (Variable name2) = compare name1 name2

instance (Monad m, MonadFail m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m) => Eval (Value s l) m s Variable where
  evaluate _ (Variable x) = do
    env <- askEnv
    maybe (fail ("free variable: " ++ x)) deref (envLookup x (env :: Environment l (Value s l)))

instance (Alternative m, MonadFail m, MonadStore Monovariant Type m, MonadEnv Monovariant Type m) => Eval Type m s Variable where
  evaluate _ (Variable x) = do
    env <- askEnv
    maybe (fail ("free type: " ++ x)) deref (envLookup x (env :: Environment Monovariant Type))

instance FreeVariables (Variable a) where
  freeVariables (Variable name) = point name


-- Primitives
newtype Primitive a = Primitive Prim deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Primitive where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Primitive where liftEq _ (Primitive a1) (Primitive a2) = a1 == a2
instance Ord1 Primitive where liftCompare _ (Primitive a1) (Primitive a2) = compare a1 a2

instance Monad m => Eval (Value s l) m s Primitive where
  evaluate _ (Primitive x) = return (I x)

instance Monad m => Eval Type m s Primitive where
  evaluate _ (Primitive (PInt _)) = return Int
  evaluate _ (Primitive (PBool _)) = return Bool


-- Lambdas
data Lambda a = Lambda Name a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Lambda where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Lambda where liftEq comp (Lambda name1 body1) (Lambda name2 body2) = name1 == name2 && comp body1 body2
instance Ord1 Lambda where liftCompare comp (Lambda name1 body1) (Lambda name2 body2) = compare name1 name2 `mappend` comp body1 body2

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

instance FreeVariables a => FreeVariables (Lambda a) where
  freeVariables (Lambda name body) = delete name (freeVariables body)


-- Recursive lambdas
data Rec a = Rec Name a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Rec where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Rec where liftEq comp (Rec name1 body1) (Rec name2 body2) = name1 == name2 && comp body1 body2
instance Ord1 Rec where liftCompare comp (Rec name1 body1) (Rec name2 body2) = compare name1 name2 `mappend` comp body1 body2

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

instance FreeVariables a => FreeVariables (Rec a) where
  freeVariables (Rec name body) = delete name (freeVariables body)


-- Application
data Application a = Application a a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Application where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Application where liftEq comp (Application a1 b1) (Application a2 b2) = comp a1 a2 && comp b1 b2
instance Ord1 Application where liftCompare comp (Application a1 b1) (Application a2 b2) = comp a1 a2 `mappend` comp b1 b2

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
         => EvalCollect (Value s l) m s Application where
  evalCollect _ ev (Application e1 e2) = do
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
         => EvalCollect Type m s Application where
  evalCollect _ ev (Application e1 e2) = do
    env <- askEnv @l @Type
    opTy <- extraRoots (envRoots env (freeVariables e2)) (ev e1)
    inTy <- extraRoots (valueRoots @l opTy) (ev e2)
    tvar <- fresh
    _ :-> outTy <- opTy `unify` (inTy :-> TVar tvar)
    return outTy


-- Unary operations
data Unary a = Unary Op1 a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Unary where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Unary where liftEq comp (Unary op1 expr1) (Unary op2 expr2) = op1 == op2 && comp expr1 expr2
instance Ord1 Unary where liftCompare comp (Unary op1 expr1) (Unary op2 expr2) = compare op1 op2 `mappend` comp expr1 expr2

instance (Monad m, MonadPrim v m) => Eval v m s Unary where
  evaluate ev (Unary op e) = do
    v <- ev e
    delta1 op v

-- Binary operations
data Binary a = Binary Op2 a a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 Binary where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Binary where liftEq comp (Binary op1 expr1A expr1B) (Binary op2 expr2A expr2B) = op1 == op2 && comp expr1A expr2A && comp expr1B expr2B
instance Ord1 Binary where liftCompare comp (Binary op1 expr1A expr1B) (Binary op2 expr2A expr2B) = compare op1 op2 `mappend` comp expr1A expr2A `mappend` comp expr1B expr2B

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
         => EvalCollect v m s Binary where
  evalCollect _ ev (Binary op e0 e1) = do
    env <- askEnv @l @v
    v0 <- extraRoots (envRoots env (freeVariables e1)) (ev e0)
    v1 <- extraRoots (valueRoots @l v0) (ev e1)
    delta2 op v0 v1


-- If statements
data If a = If a a a deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
instance Show1 If where liftShowsPrec = genericLiftShowsPrec
instance Eq1 If where liftEq comp (If c1 then1 else1) (If c2 then2 else2) = comp c1 c2 && comp then1 then2 && comp else1 else2
instance Ord1 If where liftCompare comp (If c1 then1 else1) (If c2 then2 else2) = comp c1 c2 `mappend` comp then1 then2 `mappend` comp else1 else2

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
         => EvalCollect v m s If where
  evalCollect _ ev (If c t e) = do
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

-- data Syntax a r
--   = Other (Union '[Lambda, Application, Variable, Primitive] r)
  -- | Var Name
  -- | Prim a
  -- | Op1 Op1 r
  -- | Op2 Op2 r r
  -- | App r r
  -- | Lam Name r
  -- | Rec Name r
  -- | If r r r
  -- deriving (Eq, Ord, Show)






-- instance Recursive (Term a) where
--   project = out
--
-- instance Corecursive (Term a) where
--   embed = In

-- instance Foldable Term where
--   foldMap f = go where go = bifoldMap f go . out

-- instance Bifoldable Syntax where
--   bifoldMap f g s = case s of
--     Var _ -> mempty
--     Prim i -> f i
--     -- Op1 _ a -> g a
--     -- Op2 _ a b -> g a `mappend` g b
--     App a b -> g a `mappend` g b
--     Lam _ a -> g a
--     -- Rec _ a -> g a
--     -- If c t e -> g c `mappend` g t `mappend` g e

-- instance Foldable (Syntax a) where
--   foldMap = bifoldMap (const mempty)


-- instance Functor Term where
--   fmap f = go where go = In . bimap f go . out

-- instance Bifunctor Syntax where
--   bimap f g s = case s of
--     Var n -> Var n
--     Prim v -> Prim (f v)
--     -- Op1 o a -> Op1 o (g a)
--     -- Op2 o a b -> Op2 o (g a) (g b)
--     App a b -> App (g a) (g b)
--     Lam n a -> Lam n (g a)
--     -- Rec n a -> Rec n (g a)
--     -- If c t e -> If (g c) (g t) (g e)

-- instance Functor (Syntax a) where
--   fmap = second
--
--
-- instance Traversable Term where
--   traverse f = go where go = fmap In . bitraverse f go . out

-- instance Bitraversable Syntax where
--   bitraverse f g s = case s of
--     Var n -> pure (Var n)
--     Prim v -> Prim <$> f v
--     -- Op1 o a -> Op1 o <$> g a
--     -- Op2 o a b -> Op2 o <$> g a <*> g b
--     App a b -> App <$> g a <*> g b
--     Lam n a -> Lam n <$> g a
--     -- Rec n a -> Rec n <$> g a
--     -- If c t e -> If <$> g c <*> g t <*> g e

-- instance Traversable (Syntax a) where
--   traverse = bitraverse pure
--
-- instance Num a => Num (Term a) where
--   fromInteger = prim . fromInteger
--
--   -- signum = In . Op1 Signum
--   -- abs    = In . Op1 Abs
--   -- negate = In . Op1 Negate
--   -- (+) = (In .) . Op2 Plus
--   -- (-) = (In .) . Op2 Minus
--   -- (*) = (In .) . Op2 Times
