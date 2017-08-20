{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Abstract.Number where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Text.Prettyprint.Doc
import Prelude hiding (fail)

data Op1 = Negate | Abs | Signum
  deriving (Eq, Ord, Show)

data Op2 = Plus | Minus | Times | DividedBy | Quotient | Remainder
  deriving (Eq, Ord, Show)


data N = N
  deriving (Eq, Ord, Show)


class AbstractNumber a m where
  delta1 :: Op1 -> a -> m a
  delta2 :: Op2 -> a -> a -> m a
  isZero :: a -> m Bool


divisionByZero :: MonadFail m => m a
divisionByZero = fail "division by zero"


instance MonadFail m => AbstractNumber Int m where
  delta1 o a = pure $ case o of
    Negate -> negate a
    Abs -> abs a
    Signum -> signum a

  delta2 o a b = case o of
    Plus -> return $ a + b
    Minus -> return $ a - b
    Times -> return $ a * b
    DividedBy -> do
      isZero b >>= flip when divisionByZero
      return $ a `div` b
    Quotient -> do
      isZero b >>= flip when divisionByZero
      return $ a `quot` b
    Remainder -> do
      isZero b >>= flip when divisionByZero
      return $ a `rem` b

  isZero a = pure (a == 0)

instance (Alternative m, MonadFail m) => AbstractNumber N m where
  delta1 _ N = pure N

  delta2 DividedBy _ N = pure N <|> divisionByZero
  delta2 _ _ _ = pure N

  isZero N = pure True <|> pure False

instance Num N where
  negate N = N

  signum N = N

  abs N = N

  _ + _ = N

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
