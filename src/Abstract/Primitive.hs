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

arithmeticOperators :: [Op2]
arithmeticOperators = [Plus, Minus, Times, DividedBy, Quotient, Remainder, Modulus]

booleanOperators :: [Op2]
booleanOperators = [And, Or]

relationOperators :: [Op2]
relationOperators = [Eq, Lt, LtE, Gt, GtE]


data Prim
  = PInt  {-# UNPACK #-} !Int
  | PBool {-# UNPACK #-} !Bool
  deriving (Eq, Ord, Show)

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
    Eq        -> pure (PBool (a == b))
    Lt        -> pure (PBool (a < b))
    LtE       -> pure (PBool (a <= b))
    Gt        -> pure (PBool (a > b))
    GtE       -> pure (PBool (a >= b))
    _         -> nonBoolean
  delta2 o (PBool a) (PBool b) = case o of
    And -> pure (PBool (a && b))
    Or  -> pure (PBool (a || b))
    Eq  -> pure (PBool (a == b))
    Lt  -> pure (PBool (a < b))
    LtE -> pure (PBool (a <= b))
    Gt  -> pure (PBool (a > b))
    GtE -> pure (PBool (a >= b))
    _   -> nonNumeric
  delta2 _ _ _ = disjointComparison

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


instance Num Prim where
  fromInteger = PInt . fromInteger

  negate (PInt a) = PInt (negate a)
  negate _        = error "negate of non-integer"
  abs (PInt a) = PInt (abs a)
  abs _        = error "abs of non-integer"
  signum (PInt a) = PInt (signum a)
  signum _        = error "signum of non-integer"

  PInt a + PInt b = PInt (a + b)
  _      + _      = error "(+) of non-integer"
  PInt a * PInt b = PInt (a * b)
  _      * _      = error "(*) of non-integer"


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
