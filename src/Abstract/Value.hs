{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Abstract.Value where

import Abstract.Number
import Abstract.Syntax
import Control.Monad.Fail
import qualified Data.Map as Map
import Prelude hiding (fail)

type Environment = Map.Map String

data Value l i = I i | Closure String (Term i) (Environment (l i))
  deriving (Eq, Ord, Show)


instance (MonadFail m, AbstractNumber i m) => AbstractNumber (Value l i) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"
