module Abs.Value where

import Control.Monad.Fail
import Prelude hiding (fail)

data Op1 = Negate | Abs | Signum
  deriving (Eq, Ord, Show)

data Op2 = Plus | Minus | Times | DividedBy
  deriving (Eq, Ord, Show)


data AbstractNum i = C i | N
  deriving (Eq, Ord, Show)


class AbstractValue i where
  delta1 :: MonadFail m => Op1 -> i -> m i
  delta2 :: MonadFail m => Op2 -> i -> i -> m i
  isZero :: MonadFail m => i -> m Bool


instance AbstractValue Int where
  delta1 o a = pure $ case o of
    Negate -> negate a
    Abs -> abs a
    Signum -> signum a

  delta2 o a b = case o of
    Plus -> return $ a + b
    Minus -> return $ a - b
    Times -> return $ a * b
    DividedBy -> if b == 0 then
        fail "division by zero"
      else
        return $ a `div` b

  isZero a = pure (a == 0)
