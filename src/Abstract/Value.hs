{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Abstract.Value where

import Abstract.Number
import Abstract.Syntax
import Control.Monad.Fail
import qualified Data.Map as Map
import Prelude hiding (fail)

type Environment = Map.Map String

data Value l a = I a | Closure String (Term a) (Environment (l (Value l a)))

deriving instance (Eq a, Eq (l (Value l a))) => Eq (Value l a)
deriving instance (Ord a, Ord (l (Value l a))) => Ord (Value l a)
deriving instance (Show a, Show (l (Value l a))) => Show (Value l a)


instance (MonadFail m, AbstractNumber i m) => AbstractNumber (Value l i) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"
