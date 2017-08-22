{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, UndecidableInstances #-}
module Abstract.Configuration where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Data.Functor.Classes
import Data.Functor.Classes.Pretty

data Configuration l t v = Configuration { configurationTerm :: t, configurationEnvironment :: Environment (l v), configurationStore :: Store l v }
  deriving (Foldable, Functor, Traversable)


instance Address l => Eq2 (Configuration l) where
  liftEq2 eqT eqV (Configuration t1 e1 s1) (Configuration t2 e2 s2) = eqT t1 t2 && liftEq (liftEq eqV) e1 e2 && liftEq eqV s1 s2

instance (Address l, Eq t) => Eq1 (Configuration l t) where
  liftEq = liftEq2 (==)

instance (Eq v, Eq t, Address l) => Eq (Configuration l t v) where
  (==) = eq1

instance Address l => Ord2 (Configuration l) where
  liftCompare2 compareT compareV (Configuration t1 e1 s1) (Configuration t2 e2 s2) = compareT t1 t2 <> liftCompare (liftCompare compareV) e1 e2 <> liftCompare compareV s1 s2

instance (Address l, Ord t) => Ord1 (Configuration l t) where
  liftCompare = liftCompare2 compare

instance (Ord v, Ord t, Address l) => Ord (Configuration l t v) where
  compare = compare1


instance Address l => Show2 (Configuration l) where
  liftShowsPrec2 spT _ spV slV d (Configuration t1 e1 s1) = showsTernaryWith spT (liftShowsPrec (liftShowsPrec spV slV) (liftShowList spV slV)) (liftShowsPrec spV slV) "Configuration" d t1 e1 s1

instance (Address l, Show t) => Show1 (Configuration l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show v, Show t, Address l) => Show (Configuration l t v) where
  showsPrec = showsPrec1


instance (Address l, Pretty1 l, Pretty1 (Cell l)) => Pretty2 (Configuration l) where
  liftPretty2 pT _ pV plV (Configuration t e s) = tupled [ pT t, liftPretty (liftPretty pV plV) (liftPrettyList pV plV) e, liftPretty pV plV s ]

instance (Address l, Pretty1 l, Pretty1 (Cell l), Pretty t) => Pretty1 (Configuration l t) where
  liftPretty = liftPretty2 pretty prettyList

instance (Address l, Pretty1 l, Pretty1 (Cell l), Pretty t, Pretty v) => Pretty (Configuration l t v) where
  pretty = pretty1
