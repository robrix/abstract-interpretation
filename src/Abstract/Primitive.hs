{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Abstract.Primitive where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Text.Prettyprint.Doc
import Prelude hiding (fail)

data Op1 = Negate | Abs | Signum
  deriving (Eq, Ord, Show)

data Op2 = Plus | Minus | Times | DividedBy | Quotient | Remainder
  deriving (Eq, Ord, Show)


data Prim
  = PInt Int
  | PBool Bool
  deriving (Eq, Ord, Show)

data Abstract = N | B
  deriving (Eq, Ord, Show)


class MonadFail m => Primitive a m where
  delta1 :: Op1 -> a -> m a
  delta2 :: Op2 -> a -> a -> m a
  isZero :: a -> m Bool


divisionByZero :: MonadFail m => m a
divisionByZero = fail "division by zero"

nonNumeric :: MonadFail m => m a
nonNumeric = fail "numeric operation on non-numeric value"


instance MonadFail m => Primitive Prim m where
  delta1 o (PInt a) = pure . PInt $ case o of
    Negate -> negate a
    Abs -> abs a
    Signum -> signum a
  delta1 _ _ = nonNumeric

  delta2 o (PInt a) (PInt b) = case o of
    Plus -> pure (PInt (a + b))
    Minus -> pure (PInt (a - b))
    Times -> pure (PInt (a * b))
    DividedBy -> isZero (PInt b) >>= flip when divisionByZero >> pure (PInt (a `div` b))
    Quotient ->  isZero (PInt b) >>= flip when divisionByZero >> pure (PInt (a `quot` b))
    Remainder -> isZero (PInt b) >>= flip when divisionByZero >> pure (PInt (a `rem` b))
  delta2 _ _ _ = nonNumeric

  isZero (PInt a) = pure (a == 0)
  isZero _ = nonNumeric

instance (Alternative m, MonadFail m) => Primitive Abstract m where
  delta1 _ N = pure N
  delta1 _ _ = nonNumeric

  delta2 DividedBy _ N = pure N <|> divisionByZero
  delta2 Quotient _ N = pure N <|> divisionByZero
  delta2 Remainder _ N = pure N <|> divisionByZero
  delta2 _ _ N = pure N
  delta2 _ _ _ = nonNumeric

  isZero N = pure True <|> pure False
  isZero _ = nonNumeric

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

instance Pretty Op2 where
  pretty Plus = pretty '+'
  pretty Minus = pretty '-'
  pretty Times = pretty '*'
  pretty DividedBy = pretty '/'
  pretty Quotient = pretty "`quot`"
  pretty Remainder = pretty "`rem`"
