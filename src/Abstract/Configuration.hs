{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, StandaloneDeriving #-}
module Abstract.Configuration where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Data.Functor.Classes
import Data.Semigroup

data Configuration l t v = Configuration { configurationTerm :: t, configurationEnvironment :: Environment (l v), configurationStore :: Store l v }

deriving instance (Address l, Foldable l) => Foldable (Configuration l t)
deriving instance (Address l, Functor l) => Functor (Configuration l t)
deriving instance (Address l, Traversable l) => Traversable (Configuration l t)


instance Address l => Eq2 (Configuration l) where
  liftEq2 eqT eqV (Configuration t1 e1 s1) (Configuration t2 e2 s2) = eqT t1 t2 && liftEq addressEq e1 e2 && liftEq eqV s1 s2

instance (Address l, Eq t) => Eq1 (Configuration l t) where
  liftEq = liftEq2 (==)

instance (Eq v, Eq t, Address l) => Eq (Configuration l t v) where
  (==) = eq1

instance Address l => Ord2 (Configuration l) where
  liftCompare2 compareT compareV (Configuration t1 e1 s1) (Configuration t2 e2 s2) = compareT t1 t2 <> liftCompare addressCompare e1 e2 <> liftCompare compareV s1 s2

instance (Address l, Ord t) => Ord1 (Configuration l t) where
  liftCompare = liftCompare2 compare

instance (Ord v, Ord t, Address l) => Ord (Configuration l t v) where
  compare = compare1


instance Address l => Show2 (Configuration l) where
  liftShowsPrec2 spT _ spV slV d (Configuration t1 e1 s1) = showsTernaryWith spT (liftShowsPrec addressShowsPrec addressShowList) (liftShowsPrec spV slV) "Configuration" d t1 e1 s1

instance (Address l, Show t) => Show1 (Configuration l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show v, Show t, Address l) => Show (Configuration l t v) where
  showsPrec = showsPrec1
