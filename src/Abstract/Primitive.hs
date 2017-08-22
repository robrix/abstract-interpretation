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

data N = N
  deriving (Eq, Ord, Show)


class MonadFail m => Primitive a m where
  delta1 :: Op1 -> a -> m a
  delta2 :: Op2 -> a -> a -> m a
  isZero :: a -> m Bool


divisionByZero :: MonadFail m => m a
divisionByZero = fail "division by zero"


instance MonadFail m => Primitive Prim m where
  delta1 o a = pure $ case o of
    Negate -> negate a
    Abs -> abs a
    Signum -> signum a

  delta2 o a b = case o of
    Plus -> return $ a + b
    Minus -> return $ a - b
    Times -> return $ a * b
    DividedBy -> isZero b >>= flip when divisionByZero >> return (a `div` b)
    Quotient ->  isZero b >>= flip when divisionByZero >> return (a `quot` b)
    Remainder -> isZero b >>= flip when divisionByZero >> return (a `rem` b)

  isZero a = pure (a == 0)

instance (Alternative m, MonadFail m) => Primitive N m where
  delta1 _ N = pure N

  delta2 DividedBy _ N = pure N <|> divisionByZero
  delta2 Quotient _ N = pure N <|> divisionByZero
  delta2 Remainder _ N = pure N <|> divisionByZero
  delta2 _ _ _ = pure N

  isZero N = pure True <|> pure False

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

instance Num N where
  negate N = N

  signum N = N

  abs N = N

  _ + _ = N
  _ - _ = N

  _ * _ = N

  fromInteger = const N

instance Pretty N where
  pretty N = pretty 'N'

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
