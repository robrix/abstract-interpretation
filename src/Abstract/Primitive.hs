{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abstract.Primitive where

import Abstract.Type
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Text.Prettyprint.Doc
import Prelude hiding (fail)

data Op1 = Negate | Abs | Signum | Not
  deriving (Eq, Ord, Show)

data Op2 = Plus | Minus | Times | DividedBy | Quotient | Remainder | Modulus | And | Or | Eq | Lt | LtE | Gt | GtE
  deriving (Eq, Ord, Show)


data Prim
  = PInt  {-# UNPACK #-} !Int
  | PBool {-# UNPACK #-} !Bool
  deriving (Eq, Ord, Show)

class Primitive t where
  unary :: Op1 -> t -> t

  binary :: Op2 -> t -> t -> t

  fromIntegerPrim :: Integer -> t

eq :: Primitive t => t -> t -> t
eq = binary Eq

lt :: Primitive t => t -> t -> t
lt = binary Lt

lte :: Primitive t => t -> t -> t
lte = binary LtE

gt :: Primitive t => t -> t -> t
gt = binary Gt

gte :: Primitive t => t -> t -> t
gte = binary GtE

and' :: Primitive t => t -> t -> t
and' = binary And

or' :: Primitive t => t -> t -> t
or' = binary Or

not' :: Primitive t => t -> t
not' = unary Not


class Primitive t => AbstractPrimitive a t | t -> a where
  prim :: a -> t


class Monad m => MonadPrim a m where
  delta1 :: Op1 -> a -> m a
  delta2 :: Op2 -> a -> a -> m a
  truthy :: a -> m Bool


divisionByZero :: MonadFail m => m a
divisionByZero = fail "division by zero"

nonNumeric :: MonadFail m => m a
nonNumeric = fail "numeric operation on non-numeric value"

nonBoolean :: MonadFail m => m a
nonBoolean = fail "boolean operation on non-boolean value"

disjointComparison :: MonadFail m => m a
disjointComparison = fail "comparison of disjoint values"

undefinedComparison :: MonadFail m => m a
undefinedComparison = fail "undefined comparison"


isZero :: (Num a, MonadPrim a m) => a -> m Bool
isZero = truthy <=< delta2 Eq 0

instance MonadFail m => MonadPrim Prim m where
  delta1 o a = case (o, a) of
    (Negate, PInt a)  -> pure (PInt (negate a))
    (Abs,    PInt a)  -> pure (PInt (abs a))
    (Signum, PInt a)  -> pure (PInt (signum a))
    (Not,    PBool a) -> pure (PBool (not a))
    (Not,    _)       -> nonBoolean
    _                 -> nonNumeric

  delta2 o (PInt a) (PInt b) = case o of
    Plus      -> pure (PInt (a + b))
    Minus     -> pure (PInt (a - b))
    Times     -> pure (PInt (a * b))
    DividedBy -> isZero (PInt b) >>= flip when divisionByZero >> pure (PInt (a `div` b))
    Quotient  -> isZero (PInt b) >>= flip when divisionByZero >> pure (PInt (a `quot` b))
    Remainder -> isZero (PInt b) >>= flip when divisionByZero >> pure (PInt (a `rem` b))
    Modulus   -> isZero (PInt b) >>= flip when divisionByZero >> pure (PInt (a `mod` b))
    And       -> nonBoolean
    Or        -> nonBoolean
    Eq        -> pure (PBool (a == b))
    Lt        -> pure (PBool (a < b))
    LtE       -> pure (PBool (a <= b))
    Gt        -> pure (PBool (a > b))
    GtE       -> pure (PBool (a >= b))
  delta2 And (PBool a) (PBool b) = pure (PBool (a && b))
  delta2 And _         _         = nonBoolean
  delta2 Or  (PBool a) (PBool b) = pure (PBool (a || b))
  delta2 Or  _         _         = nonBoolean
  delta2 Eq  (PBool a) (PBool b) = pure (PBool (a == b))
  delta2 Eq  _         _         = disjointComparison
  delta2 Lt  (PBool a) (PBool b) = pure (PBool (a < b))
  delta2 Lt  _         _         = disjointComparison
  delta2 LtE (PBool a) (PBool b) = pure (PBool (a <= b))
  delta2 LtE _         _         = disjointComparison
  delta2 Gt  (PBool a) (PBool b) = pure (PBool (a > b))
  delta2 Gt  _         _         = disjointComparison
  delta2 GtE (PBool a) (PBool b) = pure (PBool (a >= b))
  delta2 GtE _         _         = disjointComparison
  delta2 _ _ _ = nonNumeric

  truthy (PBool a) = pure a
  truthy _         = nonBoolean

instance (MonadFail m, Alternative m) => MonadPrim Type m where
  delta1 Not Bool = pure Bool
  delta1 Not _    = nonBoolean
  delta1 _   Int  = pure Int
  delta1 _   _    = nonNumeric

  delta2 And       Bool Bool = pure Bool
  delta2 And       _    _    = nonBoolean
  delta2 Or        Bool Bool = pure Bool
  delta2 Or        _    _    = nonBoolean
  delta2 Eq        Bool Bool = pure Bool
  delta2 Eq        Int  Int  = pure Bool
  delta2 Eq        _    _    = disjointComparison
  delta2 Lt        Bool Bool = pure Bool
  delta2 Lt        Int  Int  = pure Bool
  delta2 Lt        _    _    = disjointComparison
  delta2 LtE       Bool Bool = pure Bool
  delta2 LtE       Int  Int  = pure Bool
  delta2 LtE       _    _    = disjointComparison
  delta2 Gt        Bool Bool = pure Bool
  delta2 Gt        Int  Int  = pure Bool
  delta2 Gt        _    _    = disjointComparison
  delta2 GtE       Bool Bool = pure Bool
  delta2 GtE       Int  Int  = pure Bool
  delta2 GtE       _    _    = disjointComparison
  delta2 DividedBy Int  Int  = pure Int <|> divisionByZero
  delta2 Quotient  Int  Int  = pure Int <|> divisionByZero
  delta2 Remainder Int  Int  = pure Int <|> divisionByZero
  delta2 Modulus   Int  Int  = pure Int <|> divisionByZero
  delta2 _         Int  Int  = pure Int
  delta2 _         _    _    = nonNumeric

  truthy Bool = pure True <|> pure False
  truthy _    = nonBoolean

instance Primitive Prim where
  unary Negate (PInt a)  = PInt (negate a)
  unary Negate _         = error "negate of non-numeric primitive"
  unary Abs    (PInt a)  = PInt (abs a)
  unary Abs    _         = error "abs of non-numeric primitive"
  unary Signum (PInt a)  = PInt (signum a)
  unary Signum _         = error "signum of non-numeric primitive"

  unary Not    (PBool a) = PBool (not a)
  unary Not _            = error "not of non-boolean primitive"

  binary Plus      (PInt a) (PInt b) = PInt (a + b)
  binary Plus      _        _        = error "(+) of non-numeric primitive"
  binary Minus     (PInt a) (PInt b) = PInt (a - b)
  binary Minus     _        _        = error "(-) of non-numeric primitive"
  binary Times     (PInt a) (PInt b) = PInt (a * b)
  binary Times     _        _        = error "(*) of non-numeric primitive"
  binary DividedBy (PInt a) (PInt b) = PInt (a `div` b)
  binary DividedBy _        _        = error "(/) of non-numeric primitive"
  binary Quotient  (PInt a) (PInt b) = PInt (a `div` b)
  binary Quotient  _        _        = error "quot of non-numeric primitive"
  binary Remainder (PInt a) (PInt b) = PInt (a `div` b)
  binary Remainder _        _        = error "rem of non-numeric primitive"
  binary Modulus   (PInt a) (PInt b) = PInt (a `div` b)
  binary Modulus   _        _        = error "mod of non-numeric primitive"

  binary And (PBool a) (PBool b) = PBool (a && b)
  binary And _          _        = error "(&&) of non-boolean primitive"
  binary Or  (PBool a) (PBool b) = PBool (a || b)
  binary Or  _          _        = error "(||) of non-boolean primitive"

  binary Eq  (PInt a)  (PInt b)  = PBool (a == b)
  binary Eq  (PBool a) (PBool b) = PBool (a == b)
  binary Eq  _         _         = error "(==) of disjoint primitives"
  binary Lt  (PInt a)  (PInt b)  = PBool (a < b)
  binary Lt  (PBool a) (PBool b) = PBool (a < b)
  binary Lt  _         _         = error "(<) of disjoint primitives"
  binary LtE (PInt a)  (PInt b)  = PBool (a <= b)
  binary LtE (PBool a) (PBool b) = PBool (a <= b)
  binary LtE _         _         = error "(<=) of disjoint primitives"
  binary Gt  (PInt a)  (PInt b)  = PBool (a > b)
  binary Gt  (PBool a) (PBool b) = PBool (a > b)
  binary Gt  _         _         = error "(>) of disjoint primitives"
  binary GtE (PInt a)  (PInt b)  = PBool (a >= b)
  binary GtE (PBool a) (PBool b) = PBool (a >= b)
  binary GtE _         _         = error "(>=) of disjoint primitives"

  fromIntegerPrim = PInt . fromInteger

instance Primitive Type where
  unary Negate Int = Int
  unary Negate _   = error "negate of non-numeric primitive"
  unary Abs    Int = Int
  unary Abs    _   = error "abs of non-numeric primitive"
  unary Signum Int = Int
  unary Signum _   = error "signum of non-numeric primitive"

  unary Not    Bool = Bool
  unary Not    _    = error "not of non-boolean primitive"

  binary Plus      Int Int = Int
  binary Plus      _   _   = error "(+) of non-numeric primitive"
  binary Minus     Int Int = Int
  binary Minus     _   _   = error "(-) of non-numeric primitive"
  binary Times     Int Int = Int
  binary Times     _   _   = error "(*) of non-numeric primitive"
  binary DividedBy Int Int = Int
  binary DividedBy _   _   = error "(/) of non-numeric primitive"
  binary Quotient  Int Int = Int
  binary Quotient  _   _   = error "quot of non-numeric primitive"
  binary Remainder Int Int = Int
  binary Remainder _   _   = error "rem of non-numeric primitive"
  binary Modulus   Int Int = Int
  binary Modulus   _   _   = error "mod of non-numeric primitive"

  binary And Bool Bool = Bool
  binary And _    _    = error "(&&) of non-boolean primitive"
  binary Or  Bool Bool = Bool
  binary Or  _    _    = error "(||) of non-boolean primitive"

  binary Eq  Int  Int  = Bool
  binary Eq  Bool Bool = Bool
  binary Eq  _    _    = error "(==) of disjoint primitives"
  binary Lt  Int  Int  = Bool
  binary Lt  Bool Bool = Bool
  binary Lt  _    _    = error "(<) of disjoint primitives"
  binary LtE Int  Int  = Bool
  binary LtE Bool Bool = Bool
  binary LtE _    _    = error "(<=) of disjoint primitives"
  binary Gt  Int  Int  = Bool
  binary Gt  Bool Bool = Bool
  binary Gt  _    _    = error "(>) of disjoint primitives"
  binary GtE Int  Int  = Bool
  binary GtE Bool Bool = Bool
  binary GtE _    _    = error "(>=) of disjoint primitives"

  fromIntegerPrim _ = Int

instance Num Prim where
  negate = unary Negate
  signum = unary Signum
  abs = unary Abs

  (+) = binary Plus
  (-) = binary Minus
  (*) = binary Times

  fromInteger = fromIntegerPrim

instance Real Prim where
  toRational (PInt a) = toRational a
  toRational _ = error "toRational of non-numeric primitive"

instance Enum Prim where
  toEnum = PInt
  fromEnum (PInt a) = a
  fromEnum _ = error "fromEnum of non-numeric primitive"

instance Integral Prim where
  toInteger (PInt a) = toInteger a
  toInteger _ = error "toInteger of non-numeric primitive"
  PInt a `quotRem` PInt b = let (q, r) = a `quotRem` b in (PInt q, PInt r)
  _ `quotRem` _ = error "quotRem of non-numeric primitive"

instance Pretty Prim where
  pretty (PBool a) = pretty a
  pretty (PInt a) = pretty a

instance Pretty Op1 where
  pretty Negate = pretty "negate"
  pretty Abs = pretty "abs"
  pretty Signum = pretty "signum"
  pretty Not = pretty "not"

instance Pretty Op2 where
  pretty Plus = pretty '+'
  pretty Minus = pretty '-'
  pretty Times = pretty '*'
  pretty DividedBy = pretty '/'
  pretty Quotient = pretty "`quot`"
  pretty Remainder = pretty "`rem`"
  pretty Modulus = pretty "`mod`"
  pretty And = pretty "&&"
  pretty Or  = pretty "||"
  pretty Eq = pretty "=="
  pretty Lt = pretty "<"
  pretty LtE = pretty "<="
  pretty Gt = pretty ">"
  pretty GtE = pretty ">="
