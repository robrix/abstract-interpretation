{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Abstract.Value where

import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Control.Monad.Fail
import Data.Functor.Classes
import Data.Functor.Classes.Pretty
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)
import Text.Show

newtype Environment a = Environment { unEnvironment :: Map.Map Name a }
  deriving (Eq, Eq1, Foldable, Functor, Monoid, Ord, Ord1, Show, Show1, Traversable)

envLookup :: Name -> Environment a -> Maybe a
envLookup = (. unEnvironment) . Map.lookup

envInsert :: Name -> a -> Environment a -> Environment a
envInsert = (((Environment .) . (. unEnvironment)) .) . Map.insert


data Value l t a
  = I a
  | Closure Name t (Environment (l (Value l t a)))
  deriving (Foldable, Functor, Traversable)


instance Address l => Eq2 (Value l) where
  liftEq2 eqT eqA = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> a `eqA` b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> s1 == s2 && eqT t1 t2 && liftEq (liftEq go) e1 e2
            _ -> False

instance (Address l, Eq t) => Eq1 (Value l t) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq t, Address l) => Eq (Value l t a) where
  (==) = eq1

instance Address l => Ord2 (Value l) where
  liftCompare2 compareT compareA = go
    where go v1 v2 = case (v1, v2) of
            (I a, I b) -> compareA a b
            (Closure s1 t1 e1, Closure s2 t2 e2) -> compare s1 s2 <> compareT t1 t2 <> liftCompare (liftCompare go) e1 e2
            (I _, _) -> LT
            _ -> GT

instance (Address l, Ord t) => Ord1 (Value l t) where
  liftCompare = liftCompare2 compare

instance (Ord a, Ord t, Address l) => Ord (Value l t a) where
  compare = compare1


instance Address l => Show2 (Value l) where
  liftShowsPrec2 spT _ spA _ = go
    where go d v = case v of
            I a -> showsUnaryWith spA "I" d a
            Closure s t e -> showsTernaryWith showsPrec spT (liftShowsPrec (liftShowsPrec go (showListWith (go 0))) (liftShowList go (showListWith (go 0)))) "Closure" d s t e

instance (Address l, Show t) => Show1 (Value l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show t, Address l) => Show (Value l t a) where
  showsPrec = showsPrec1


instance Pretty1 Environment where
  liftPretty p pl = liftPrettyList p pl . Map.toList . unEnvironment

instance Pretty a => Pretty (Environment a) where
  pretty = pretty1

instance Pretty1 l => Pretty2 (Value l) where
  liftPretty2 pT _ pA _ = go
    where go (I a) = pA a
          go (Closure n t e) = pretty n <> colon <+> pT t <> line
                                <> liftPretty (liftPretty go (list . map go)) (liftPrettyList go (list . map go)) e

instance (Pretty1 l, Pretty t) => Pretty1 (Value l t) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty1 l, Pretty t, Pretty a) => Pretty (Value l t a) where
  pretty = pretty1


instance (MonadFail m, Primitive a m) => Primitive (Value l t a) m where
  delta1 o   (I a) = fmap I (delta1 o a)
  delta1 Not _     = nonBoolean
  delta1 _   _     = nonNumeric

  delta2 o   (I a)     (I b)     = fmap I (delta2 o a b)
  delta2 And _         _         = nonBoolean
  delta2 Or  _         _         = nonBoolean
  delta2 XOr _         _         = nonBoolean
  delta2 Eq  Closure{} Closure{} = fail "equality on closures is undefined"
  delta2 _   _         _         = nonNumeric

  isZero (I a) = isZero a
  isZero _     = nonNumeric

  truthy (I a) = truthy a
  truthy _     = nonBoolean
