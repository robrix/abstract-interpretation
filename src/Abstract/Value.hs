{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Abstract.Value where

import Abstract.Number
import Abstract.Store
import Abstract.Syntax
import Control.Monad.Fail
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)

newtype Environment a = Environment { unEnvironment :: Map.Map Name a }
  deriving (Eq, Eq1, Foldable, Functor, Monoid, Ord, Ord1, Show, Show1, Traversable)

envLookup :: Name -> Environment a -> Maybe a
envLookup = (. unEnvironment) . Map.lookup

envInsert :: Name -> a -> Environment a -> Environment a
envInsert = (((Environment .) . (. unEnvironment)) .) . Map.insert


data Value l a = I a | Closure Name (Term a) (Environment (l (Value l a)))
  deriving (Foldable, Functor, Traversable)


instance Address l => Eq1 (Value l) where
  liftEq eq = go
    where go (I a) (I b) = a `eq` b
          go (Closure s1 t1 e1) (Closure s2 t2 e2) = s1 == s2 && liftEqTerms eq t1 t2 && liftEq addressEq e1 e2
          go _ _ = False

instance (Eq a, Address l) => Eq (Value l a) where
  (==) = eq1

instance Address l => Ord1 (Value l) where
  liftCompare compareA = go
    where go (I a) (I b) = compareA a b
          go (Closure s1 t1 e1) (Closure s2 t2 e2) = compare s1 s2 <> liftCompareTerms compareA t1 t2 <> liftCompare addressCompare e1 e2
          go (I _) _ = LT
          go _ _ = GT

instance (Ord a, Address l) => Ord (Value l a) where
  compare = compare1


instance Address l => Show1 (Value l) where
  liftShowsPrec spA spL = go
    where go d (I a) = showsUnaryWith spA "I" d a
          go d (Closure s t e) = showsTernaryWith showsPrec (liftShowsPrecTerm spA spL) (liftShowsPrec addressShowsPrec addressShowList) "Closure" d s t e

instance (Show a, Address l) => Show (Value l a) where
  showsPrec = showsPrec1


instance (MonadFail m, AbstractNumber a m) => AbstractNumber (Value l a) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"
