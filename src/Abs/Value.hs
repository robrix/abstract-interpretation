{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Abs.Value where

import Control.Monad.Fail
import Prelude hiding (fail)

data Op1 = Negate | Abs | Signum
  deriving (Eq, Ord, Show)

data Op2 = Plus | Minus | Times | DividedBy
  deriving (Eq, Ord, Show)


data AbstractNum i = C i | N
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
    DividedBy -> if b == 0 then
        divisionByZero
      else
        return $ a `div` b

  isZero a = pure (a == 0)
