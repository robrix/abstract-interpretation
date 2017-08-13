{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module Abstract.Configuration where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Data.Functor.Classes
import Data.Semigroup

data Configuration l a = Configuration { configurationTerm :: Term a, configurationEnvironment :: Environment (l (Value l a)), configurationStore :: Store l (Value l a) }

instance Address l => Eq1 (Configuration l) where
  liftEq :: forall a b . (a -> b -> Bool) -> Configuration l a -> Configuration l b -> Bool
  liftEq eq (Configuration t1 e1 s1) (Configuration t2 e2 s2) = liftEqTerms eq t1 t2 && liftEq addressEq e1 e2 && liftEqStore (undefined :: proxy l) (liftEq eq :: Value l a -> Value l b -> Bool) s1 s2

instance (Eq a, Address l) => Eq (Configuration l a) where
  (==) = eq1

instance Address l => Ord1 (Configuration l) where
  liftCompare :: forall a b . (a -> b -> Ordering) -> Configuration l a -> Configuration l b -> Ordering
  liftCompare compareA (Configuration t1 e1 s1) (Configuration t2 e2 s2) = liftCompareTerms compareA t1 t2 <> liftCompare addressCompare e1 e2 <> liftCompareStore (undefined :: proxy l) (liftCompare compareA :: Value l a -> Value l b -> Ordering) s1 s2

instance (Ord a, Address l) => Ord (Configuration l a) where
  compare = compare1


instance Address l => Show1 (Configuration l) where
  liftShowsPrec :: forall a. (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Configuration l a -> ShowS
  liftShowsPrec spA slA d (Configuration t1 e1 s1) = showsTernaryWith (liftShowsPrecTerm spA slA) (liftShowsPrec addressShowsPrec addressShowList) (liftShowsPrecStore (undefined :: proxy l) (liftShowsPrec spA slA :: Int -> Value l a -> ShowS) (liftShowList spA slA :: [Value l a] -> ShowS)) "Configuration" d t1 e1 s1

instance (Show a, Address l) => Show (Configuration l a) where
  showsPrec = showsPrec1
