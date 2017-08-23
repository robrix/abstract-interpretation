{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Abstract.Primitive where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Text.Prettyprint.Doc
import Prelude hiding (fail)

data Op1 = Negate | Abs | Signum | Not
  deriving (Eq, Ord, Show)

data Op2 = Plus | Minus | Times | DividedBy | Quotient | Remainder | Modulus | And | Or | XOr | Eq | Lt | LtE | Gt | GtE
  deriving (Eq, Ord, Show)


data Prim
  = PInt  {-# UNPACK #-} !Int
  | PBool {-# UNPACK #-} !Bool
  deriving (Eq, Ord, Show)

data Abstract = N | B
  deriving (Eq, Ord, Show)


class (Num a, Num t) => AbstractNum a t | t -> a where
  prim :: a -> t


class MonadFail m => Primitive a m where
  delta1 :: Op1 -> a -> m a
  delta2 :: Op2 -> a -> a -> m a
  isZero :: a -> m Bool
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


instance MonadFail m => Primitive Prim m where
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
    XOr       -> nonBoolean
    Eq        -> pure (PBool (a == b))
    Lt        -> pure (PBool (a < b))
    LtE       -> pure (PBool (a <= b))
    Gt        -> pure (PBool (a > b))
    GtE       -> pure (PBool (a >= b))
  delta2 And (PBool a) (PBool b) = pure (PBool (a && b))
  delta2 And _         _         = nonBoolean
  delta2 Or  (PBool a) (PBool b) = pure (PBool (a || b))
  delta2 Or  _         _         = nonBoolean
  delta2 XOr (PBool a) (PBool b) = pure (PBool ((a || b) && not (a && b)))
  delta2 XOr _         _         = nonBoolean
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

  isZero (PInt a) = pure (a == 0)
  isZero _        = nonNumeric

  truthy (PBool a) = pure a
  truthy _         = nonBoolean

instance (Alternative m, MonadFail m) => Primitive Abstract m where
  delta1 Not B = pure B
  delta1 Not _ = nonBoolean
  delta1 _   N = pure N
  delta1 _   _ = nonNumeric

  delta2 And       B B = pure B
  delta2 And       _ _ = nonBoolean
  delta2 Or        B B = pure B
  delta2 Or        _ _ = nonBoolean
  delta2 XOr       B B = pure B
  delta2 XOr       _ _ = nonBoolean
  delta2 Eq        B B = pure B
  delta2 Eq        N N = pure B
  delta2 Eq        _ _ = disjointComparison
  delta2 Lt        B B = pure B
  delta2 Lt        N N = pure B
  delta2 Lt        _ _ = disjointComparison
  delta2 LtE       B B = pure B
  delta2 LtE       N N = pure B
  delta2 LtE       _ _ = disjointComparison
  delta2 Gt        B B = pure B
  delta2 Gt        N N = pure B
  delta2 Gt        _ _ = disjointComparison
  delta2 GtE       B B = pure B
  delta2 GtE       N N = pure B
  delta2 GtE       _ _ = disjointComparison
  delta2 DividedBy N N = pure N <|> divisionByZero
  delta2 Quotient  N N = pure N <|> divisionByZero
  delta2 Remainder N N = pure N <|> divisionByZero
  delta2 Modulus   N N = pure N <|> divisionByZero
  delta2 _         N N = pure N
  delta2 _         _ _ = nonNumeric

  isZero N = pure True <|> pure False
  isZero _ = nonNumeric

  truthy B = pure True <|> pure False
  truthy _ = nonBoolean

instance Num Prim where
  negate (PInt i) = PInt (negate i)
  negate _ = error "negate of non-numeric primitive"
  signum (PInt i) = PInt (signum i)
  signum _ = error "signum of non-numeric primitive"
  abs (PInt i) = PInt (abs i)
  abs _ = error "abs of non-numeric primitive"

  PInt a + PInt b = PInt (a + b)
  _ + _ = error "(+) of non-numeric primitive"
  PInt a - PInt b = PInt (a - b)
  _ - _ = error "(-) of non-numeric primitive"
  PInt a * PInt b = PInt (a * b)
  _ * _ = error "(*) of non-numeric primitive"

  fromInteger = PInt . fromInteger

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

instance Num Abstract where
  negate N = N
  negate _ = error "negate of non-numeric primitive"

  signum N = N
  signum _ = error "signum of non-numeric primitive"

  abs N = N
  abs _ = error "abs of non-numeric primitive"

  N + N = N
  _ + _ = error "(+) of non-numeric primitive"
  N - N = N
  _ - _ = error "(-) of non-numeric primitive"

  N * N = N
  _ * _ = error "(*) of non-numeric primitive"

  fromInteger = const N

instance Pretty Abstract where
  pretty N = pretty 'N'
  pretty B = pretty 'B'

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
  pretty XOr = pretty "`xor`"
  pretty Eq = pretty "=="
  pretty Lt = pretty "<"
  pretty LtE = pretty "<="
  pretty Gt = pretty ">"
  pretty GtE = pretty ">="
