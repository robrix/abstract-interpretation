{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Abstract.Configuration where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Data.Functor.Classes
import Data.Text.Prettyprint.Doc

data Configuration l t v = Configuration { configurationTerm :: t, configurationEnvironment :: Environment (Key l v), configurationStore :: Store l v }
  -- deriving (Foldable, Functor, Traversable)

deriving instance (Ord l, Functor (Cell l)) => Functor (Configuration l t)

instance (Eq l, Eq1 (Cell l)) => Eq2 (Configuration l) where
  liftEq2 eqT eqV (Configuration t1 e1 s1) (Configuration t2 e2 s2) = eqT t1 t2 && liftEq (liftEq eqV) e1 e2 && liftEq eqV s1 s2

instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Configuration l t) where
  liftEq = liftEq2 (==)

instance (Eq l, Eq t, Eq v, Eq1 (Cell l)) => Eq (Configuration l t v) where
  (==) = eq1

instance (Ord l, Ord1 (Cell l)) => Ord2 (Configuration l) where
  liftCompare2 compareT compareV (Configuration t1 e1 s1) (Configuration t2 e2 s2) = compareT t1 t2 <> liftCompare (liftCompare compareV) e1 e2 <> liftCompare compareV s1 s2

instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Configuration l t) where
  liftCompare = liftCompare2 compare

instance (Ord l, Ord t, Ord v, Ord1 (Cell l)) => Ord (Configuration l t v) where
  compare = compare1


instance (Show l, Show1 (Cell l)) => Show2 (Configuration l) where
  liftShowsPrec2 spT _ spV slV d (Configuration t1 e1 s1) = showsConstructor "Configuration" d [ flip spT t1, flip (liftShowsPrec (liftShowsPrec spV slV) (liftShowList spV slV)) e1, flip (liftShowsPrec spV slV) s1 ]

instance (Show l, Show t, Show1 (Cell l)) => Show1 (Configuration l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show l, Show t, Show v, Show1 (Cell l)) => Show (Configuration l t v) where
  showsPrec = showsPrec1


instance (Pretty l, Pretty1 (Cell l)) => Pretty2 (Configuration l) where
  liftPretty2 pT _ pV plV (Configuration t e s) = tupled [ pT t, liftPretty (liftPretty pV plV) (list . map (liftPretty pV plV)) e, liftPretty pV plV s ]

instance (Pretty l, Pretty t, Pretty1 (Cell l)) => Pretty1 (Configuration l t) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty l, Pretty t, Pretty v, Pretty1 (Cell l)) => Pretty (Configuration l t v) where
  pretty = liftPretty pretty prettyList
