{-# LANGUAGE DeriveFoldable, GeneralizedNewtypeDeriving #-}
module Abstract.RootSet where

import Abstract.Set
import Abstract.Store
import Data.Functor.Classes
import Data.Semigroup
import Data.Text.Prettyprint.Doc


newtype RootSet l a = RootSet { unRootSet :: Set (Address l a) }
  deriving (Eq, Foldable, Monoid, Ord, Semigroup, Show)


instance Eq2 RootSet where
  liftEq2 eqL eqA (RootSet s1) (RootSet s2) = liftEq (liftEq2 eqL eqA) s1 s2

instance Eq l => Eq1 (RootSet l) where
  liftEq = liftEq2 (==)

instance Ord2 RootSet where
  liftCompare2 compareL compareA (RootSet s1) (RootSet s2) = liftCompare (liftCompare2 compareL compareA) s1 s2

instance Ord l => Ord1 (RootSet l) where
  liftCompare = liftCompare2 compare


instance Show2 RootSet where
  liftShowsPrec2 spL slL spA slA d (RootSet s) = showsUnaryWith (liftShowsPrec (liftShowsPrec2 spL slL spA slA) (liftShowList2 spL slL spA slA)) "RootSet" d s

instance Show l => Show1 (RootSet l) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList


instance Pretty2 RootSet where
  liftPretty2 pL plL pA plA = liftPretty prettySet (list . map prettySet) . unRootSet
    where prettySet = liftPretty2 pL plL pA plA

instance Pretty l => Pretty1 (RootSet l) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty l, Pretty a) => Pretty (RootSet l a) where
  pretty = liftPretty pretty prettyList
