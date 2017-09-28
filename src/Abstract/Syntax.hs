{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, AllowAmbiguousTypes, DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Abstract.Syntax where

import Abstract.Environment
import Abstract.Primitive
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
import Data.Functor.Foldable hiding (fold)
import Data.Proxy
import Data.Semigroup
import Data.Union
import GHC.Generics
import Prelude hiding (fail)

type Syntax = Union '[Variable, Primitive, Lambda, Application]

type instance Base (Term syntax) = Syntax

-- Syntax Eval instances
instance (Monad m, MonadFail m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m, Semigroup (Cell l (Value s l))) => Eval (Value s l) m s Syntax where
  evaluate ev = apply (Proxy :: Proxy (Eval (Value s l) m s)) (evaluate ev)

instance (Alternative m, MonadFresh m, MonadFail m, MonadStore Monovariant Type m, MonadEnv Monovariant Type m, Semigroup (Cell Monovariant Type)) => Eval Type m s Syntax where
  evaluate ev = apply (Proxy :: Proxy (Eval Type m s)) (evaluate ev)


-- Variables
newtype Variable a = Variable String deriving (Eq, Ord, Show, Functor, Generic1)
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


-- Primitives
newtype Primitive a = Primitive Prim deriving (Eq, Ord, Show, Functor, Generic1)
instance Show1 Primitive where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Primitive where liftEq _ (Primitive a1) (Primitive a2) = a1 == a2
instance Ord1 Primitive where liftCompare _ (Primitive a1) (Primitive a2) = compare a1 a2

instance Monad m => Eval (Value s l) m s Primitive where
  evaluate _ (Primitive x) = return (I x)

instance Monad m => Eval Type m s Primitive where
  evaluate _ (Primitive (PInt _)) = return Int
  evaluate _ (Primitive (PBool _)) = return Bool


-- Lambdas
data Lambda a = Lambda Name a deriving (Eq, Ord, Show, Functor, Generic1)
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


-- Applications
data Application a = Application a a deriving (Eq, Ord, Show, Functor, Generic1)
instance Show1 Application where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Application where liftEq comp (Application a1 b1) (Application a2 b2) = comp a1 a2 && comp b1 b2
instance Ord1 Application where liftCompare comp (Application a1 b1) (Application a2 b2) = comp a1 a2 `mappend` comp b1 b2

instance (Monad m, MonadFail m, MonadAddress l m, MonadStore l (Value s l) m, MonadEnv l (Value s l) m, Semigroup (Cell l (Value s l))) => Eval (Value s l) m s Application where
  evaluate ev (Application e1 e2) = do
    Closure name body env <- ev e1
    value <- ev e2
    a <- alloc name
    assign a value
    localEnv (const (envInsert name a env)) (ev body)

instance (Monad m, MonadFail m, MonadFresh m) => Eval Type m s Application where
  evaluate ev (Application e1 e2) = do
    opTy <- ev e1
    inTy <- ev e2
    tvar <- fresh
    _ :-> outTy <- opTy `unify` (inTy :-> TVar tvar)
    return outTy


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

-- eq :: Term a -> Term a -> Term a
-- eq = (In .) . Op2 Eq
--
-- lt :: Term a -> Term a -> Term a
-- lt = (In .) . Op2 Lt
--
-- lte :: Term a -> Term a -> Term a
-- lte = (In .) . Op2 LtE
--
-- gt :: Term a -> Term a -> Term a
-- gt = (In .) . Op2 Gt
--
-- gte :: Term a -> Term a -> Term a
-- gte = (In .) . Op2 GtE
--
-- and' :: Term a -> Term a -> Term a
-- and' = (In .) . Op2 And
--
-- or' :: Term a -> Term a -> Term a
-- or' = (In .) . Op2 Or
--
-- not' :: Term a -> Term a
-- not' = In . Op1 Not
--
-- div' :: Term a -> Term a -> Term a
-- div' = (In .) . Op2 DividedBy
--
-- quot' :: Term a -> Term a -> Term a
-- quot' = (In .) . Op2 Quotient
--
-- rem' :: Term a -> Term a -> Term a
-- rem' = (In .) . Op2 Remainder
--
-- mod' :: Term a -> Term a -> Term a
-- mod' = (In .) . Op2 Modulus

-- mu :: Name -> (Term a -> Term a) -> Term a
-- mu f b = makeRec f (b (var f))

-- makeRec :: Name -> Term a -> Term a
-- makeRec name body = In (Rec name body)
--
-- if' :: Term a -> Term a -> Term a -> Term a
-- if' c t e = In (If c t e)

-- let' :: Name -> Term a -> (Term a -> Term a) -> Term a
-- let' var val body = lam var body # val

--
-- freeVariables :: Term a -> Set Name
-- freeVariables = cata (\ syntax -> case syntax of
--   Var n -> point n
--   Lam n body -> delete n body
--   -- Rec n body -> delete n body
--   _ -> fold syntax)
--



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
