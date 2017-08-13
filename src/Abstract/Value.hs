{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Abstract.Value where

import Abstract.Number
import Abstract.Syntax
import Control.Monad.Fail
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)

type Environment = Map.Map String

data Value l a = I a | Closure String (Term a) (Environment (l (Value l a)))

deriving instance (Show a, Show (l (Value l a))) => Show (Value l a)


instance Eq1 l => Eq1 (Value l) where
  liftEq eq = go
    where go (I a) (I b) = a `eq` b
          go (Closure s1 t1 e1) (Closure s2 t2 e2) = s1 == s2 && liftEqTerms eq t1 t2 && liftEq (liftEq go) e1 e2
          go _ _ = False

instance (Eq a, Eq1 l) => Eq (Value l a) where
  (==) = eq1

instance Ord1 l => Ord1 (Value l) where
  liftCompare compareA = go
    where go (I a) (I b) = compareA a b
          go (Closure s1 t1 e1) (Closure s2 t2 e2) = compare s1 s2 <> liftCompareTerms compareA t1 t2 <> liftCompare (liftCompare go) e1 e2
          go (I _) _ = LT
          go _ _ = GT

instance (Ord a, Ord1 l) => Ord (Value l a) where
  compare = compare1

instance (MonadFail m, AbstractNumber i m) => AbstractNumber (Value l i) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"
