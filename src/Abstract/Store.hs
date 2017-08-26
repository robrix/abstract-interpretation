{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Store
( Precise(..)
, Monovariant(..)
, Address(..)
, Store(..)
, Key(..)
, assign
) where

import Abstract.Syntax
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
import Data.Foldable (asum)
import Data.Function (on)
import Data.Functor.Alt
import Data.Functor.Classes
import Data.Kind
import qualified Data.Map as Map
import Data.Pointed
import Data.Semigroup
import Data.Text.Prettyprint.Doc
import Prelude hiding (fail)
import Text.Show

newtype Store l a = Store { unStore :: Map.Map (Key l a) (Cell l a) }

newtype Key l a = Key { unKey :: l }

storeLookup :: Address l => Key l a -> Store l a -> Maybe (Cell l a)
storeLookup = (. unStore) . Map.lookup

storeInsert :: Address l => Key l a -> a -> Store l a -> Store l a
storeInsert = (((Store .) . (. unStore)) .) . (. point) . Map.insertWith (<!>)

storeSize :: Store l a -> Int
storeSize = Map.size . unStore

assign :: (State (Store l a) :< fs, Address l) => Key l a -> a -> Eff fs ()
assign = (modify .) . storeInsert


class (Eq l, Ord l, Show l, Eq1 (Cell l), Ord1 (Cell l), Show1 (Cell l), Traversable (Cell l), Alt (Cell l), Pointed (Cell l)) => Address l where
  type Cell l :: * -> *
  type Context l (m :: * -> *) :: Constraint
  type instance Context l m = ()

  deref :: (State (Store l a) :< fs, MonadFail (Eff fs), Context l (Eff fs)) => Key l a -> Eff fs a

  alloc :: (State (Store l a) :< fs, MonadFail (Eff fs), Context l (Eff fs)) => Name -> Eff fs (Key l a)


newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

allocPrecise :: Store Precise a -> Key Precise a
allocPrecise = Key . Precise . storeSize

newtype I a = I { unI :: a }
  deriving (Eq, Ord, Show)

instance Address Precise where
  type Cell Precise = I

  deref = maybe uninitializedAddress (pure . unI) <=< flip fmap get . storeLookup

  alloc _ = fmap allocPrecise get


newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance Address Monovariant where
  type Cell Monovariant = []
  type Context Monovariant m = (Alternative m)

  deref = maybe uninitializedAddress (asum . fmap pure) <=< flip fmap get . storeLookup

  alloc = pure . Key . Monovariant


uninitializedAddress :: MonadFail m => m a
uninitializedAddress = fail "uninitialized address"


instance Semigroup (I a) where
  (<>) = const

instance Foldable I where
  foldMap f = f . unI

instance Functor I where
  fmap f = I . f . unI

instance Traversable I where
  traverse f = fmap I . f . unI

instance Alt I where
  a <!> _ = a

instance Pointed I where
  point = I

instance Eq1 I where
  liftEq eq (I a) (I b) = eq a b

instance Ord1 I where
  liftCompare compare (I a) (I b) = compare a b

instance Show1 I where
  liftShowsPrec sp _ d (I a) = sp d a

instance Pretty1 I where
  liftPretty p _ (I a) = p a

instance Address l => Foldable (Key l) where
  foldMap _ = mempty

instance Functor (Key l) where
  fmap _ = Key . unKey

instance Address l => Traversable (Key l) where
  traverse _ = fmap Key . pure . unKey


instance Address l => Foldable (Store l) where
  foldMap = (. unStore) . foldMap . foldMap

instance Address l => Functor (Store l) where
  fmap f = Store . Map.mapKeys (Key . unKey) . fmap (fmap f) . unStore

instance Address l => Traversable (Store l) where
  traverse f = fmap (Store . Map.mapKeys (Key . unKey)) . traverse (traverse f) . unStore


instance Address l => Monoid (Store l a) where
  mempty = Store mempty
  mappend = (Store .) . (mappend `on` unStore)


instance Address l => Eq1 (Store l) where
  liftEq eq (Store m1) (Store m2) = liftEq2 (liftEq eq) (liftEq eq) m1 m2

instance (Eq a, Address l) => Eq (Store l a) where
  (==) = eq1

instance Address l => Eq1 (Key l) where
  liftEq _ (Key a) (Key b) = a == b

instance Address l => Eq (Key l a) where
  (==) = liftEq (const (const True))

instance Address l => Ord1 (Store l) where
  liftCompare compareA (Store m1) (Store m2) = liftCompare2 (liftCompare compareA) (liftCompare compareA) m1 m2

instance (Ord a, Address l) => Ord (Store l a) where
  compare = compare1

instance Address l => Ord1 (Key l) where
  liftCompare _ (Key a) (Key b) = compare a b

instance Address l => Ord (Key l a) where
  compare = liftCompare (const (const EQ))

instance Address l => Show1 (Store l) where
  liftShowsPrec sp sl d (Store m) = showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Store" d m

instance (Show a, Address l) => Show (Store l a) where
  showsPrec = showsPrec1

instance Address l => Show1 (Key l) where
  liftShowsPrec _ _ d = showsUnaryWith showsPrec "Key" d . unKey

instance Address l => Show (Key l a) where
  showsPrec = liftShowsPrec (const (const id)) (showListWith (const id))

instance Pretty Precise where
  pretty (Precise n) = pretty "Precise" <+> pretty n

instance Pretty Monovariant where
  pretty (Monovariant n) = pretty "Monovariant" <+> pretty n

instance (Pretty l, Pretty1 (Cell l)) => Pretty1 (Store l) where
  liftPretty p pl = list . map (liftPretty (liftPretty p pl) (list . map (liftPretty p pl))) . Map.toList . unStore

instance (Pretty l, Pretty1 (Cell l), Pretty a) => Pretty (Store l a) where
  pretty = liftPretty pretty prettyList

instance Pretty l => Pretty1 (Key l) where
  liftPretty _ _ (Key l) = pretty l

instance Pretty l => Pretty (Key l a) where
  pretty = liftPretty (const emptyDoc) (const emptyDoc)
