{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Abstract.GarbageCollection where

import Abstract.Set
import Abstract.Store
import Data.Semigroup

newtype Roots l a = Roots { unRoots :: Set (Address l a) }
  deriving (Eq, Foldable, Monoid, Ord, Semigroup, Show)

class MonadGC l a m where
  askRoots :: m (Set (Address l a))
