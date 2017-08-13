{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Abstract.Number where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Prelude hiding (fail)

data Op1 = Negate | Abs | Signum
  deriving (Eq, Ord, Show)

data Op2 = Plus | Minus | Times | DividedBy | Quotient | Remainder
  deriving (Eq, Ord, Show)


data AbstractNum = N
  deriving (Eq, Ord, Show)


class AbstractValue i m where
  delta1 :: Op1 -> i -> m i
  delta2 :: Op2 -> i -> i -> m i
  isZero :: i -> m Bool


divisionByZero :: MonadFail m => m a
divisionByZero = fail "division by zero"


instance MonadFail m => AbstractValue Int m where
  delta1 o a = pure $ case o of
    Negate -> negate a
    Abs -> abs a
    Signum -> signum a

  delta2 o a b = case o of
    Plus -> return $ a + b
    Minus -> return $ a - b
    Times -> return $ a * b
    DividedBy -> do
      when (b == 0) divisionByZero
      return $ a `div` b
    Quotient -> do
      when (b == 0) divisionByZero
      return $ a `quot` b
    Remainder -> do
      when (b == 0) divisionByZero
      return $ a `rem` b

  isZero a = pure (a == 0)

instance (Alternative m, MonadFail m) => AbstractValue AbstractNum m where
  delta1 _ N = pure N

  delta2 DividedBy _ N = pure N <|> divisionByZero
  delta2 _ _ _ = pure N

  isZero N = pure True <|> pure False

instance Num AbstractNum where
  negate N = N

  signum N = N

  abs N = N

  _ + _ = N

  _ * _ = N

  fromInteger = const N
