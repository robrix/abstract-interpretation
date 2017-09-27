{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Abstract.Environment where

import Abstract.Store
import Abstract.Term

import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup
import qualified Data.Map as Map

newtype Environment l a = Environment { unEnvironment :: Map.Map Name (Address l a) }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

envLookup :: Name -> Environment l a -> Maybe (Address l a)
envLookup = (. unEnvironment) . Map.lookup

envInsert :: Name -> Address l a -> Environment l a -> Environment l a
envInsert name value (Environment m) = Environment (Map.insert name value m)

envRoots :: (Foldable t, Ord l) => Environment l a -> t Name -> Set (Address l a)
envRoots env = foldr ((<>) . maybe mempty point . flip envLookup env) mempty


-- Instances
instance Eq2 Environment where
  liftEq2 eqL eqA (Environment m1) (Environment m2) = liftEq (liftEq2 eqL eqA) m1 m2

instance Eq l => Eq1 (Environment l) where
  liftEq = liftEq2 (==)

instance Ord2 Environment where
  liftCompare2 compareL compareA (Environment m1) (Environment m2) = liftCompare (liftCompare2 compareL compareA) m1 m2

instance Ord l => Ord1 (Environment l) where
  liftCompare = liftCompare2 compare
