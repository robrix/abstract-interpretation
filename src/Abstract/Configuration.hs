{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, StandaloneDeriving #-}
module Abstract.Configuration where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Data.Functor.Classes
import Data.Semigroup

data Configuration l t a = Configuration { configurationTerm :: t, configurationEnvironment :: Environment (l (Value l t a)), configurationStore :: AddressStore l (Value l t a) }

deriving instance (Address l, Foldable l, Foldable (AddressStore l)) => Foldable (Configuration l t)
deriving instance (Address l, Functor l, Functor (AddressStore l)) => Functor (Configuration l t)
deriving instance (Address l, Traversable l, Traversable (AddressStore l)) => Traversable (Configuration l t)


instance Address l => Eq2 (Configuration l) where
  liftEq2 eqT eqA (Configuration t1 e1 s1) (Configuration t2 e2 s2) = eqT t1 t2 && liftEq addressEq e1 e2 && liftEq (liftEq2 eqT eqA) s1 s2

instance (Address l, Eq t) => Eq1 (Configuration l t) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq t, Address l) => Eq (Configuration l t a) where
  (==) = eq1

instance Address l => Ord2 (Configuration l) where
  liftCompare2 compareT compareA (Configuration t1 e1 s1) (Configuration t2 e2 s2) = compareT t1 t2 <> liftCompare addressCompare e1 e2 <> liftCompare (liftCompare2 compareT compareA) s1 s2

instance (Address l, Ord t) => Ord1 (Configuration l t) where
  liftCompare = liftCompare2 compare

instance (Ord a, Ord t, Address l) => Ord (Configuration l t a) where
  compare = compare1


instance Address l => Show2 (Configuration l) where
  liftShowsPrec2 spT slT spA slA d (Configuration t1 e1 s1) = showsTernaryWith spT (liftShowsPrec addressShowsPrec addressShowList) (liftShowsPrec (liftShowsPrec2 spT slT spA slA) (liftShowList2 spT slT spA slA)) "Configuration" d t1 e1 s1

instance (Address l, Show t) => Show1 (Configuration l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show t, Address l) => Show (Configuration l t a) where
  showsPrec = showsPrec1
