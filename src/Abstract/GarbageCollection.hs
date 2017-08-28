{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Abstract.GarbageCollection where

import Abstract.Set
import Abstract.Store
import Data.Functor.Classes
import Data.Semigroup

newtype Roots l a = Roots { unRoots :: Set (Address l a) }
  deriving (Eq, Foldable, Monoid, Ord, Semigroup, Show)

class MonadGC l a m where
  askRoots :: m (Roots l a)


instance Eq2 Roots where
  liftEq2 eqL eqA (Roots s1) (Roots s2) = liftEq (liftEq2 eqL eqA) s1 s2

instance Eq l => Eq1 (Roots l) where
  liftEq = liftEq2 (==)
