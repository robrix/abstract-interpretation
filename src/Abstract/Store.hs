{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Abstract.Store
( Precise(..)
, Monovariant(..)
, Address(..)
, addressEq
, addressCompare
, addressShowsPrec
, addressShowList
) where

import Abstract.Syntax
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
import Data.Foldable (asum)
import Data.Function (on)
import Data.Functor.Classes
import Data.Functor.Identity
import qualified Data.IntMap as IntMap
import Data.Kind
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)
import Text.Show

newtype Store l a = Store { unStore :: Map.Map (Key l a) (Cell l a) }

newtype Key l a = Key { unKey :: l a }

storeLookup :: Address l => l a -> Store l a -> Maybe (Cell l a)
storeLookup = (. unStore) . Map.lookup . Key

storeInsert :: (Ord a, Semigroup (Cell l a), Address l) => l a -> a -> Store l a -> Store l a
storeInsert = (((Store .) . (. unStore)) .) . (. pure) . Map.insertWith (<>) . Key


class (Traversable l, Eq1 l, Ord1 l, Show1 l, Eq1 (AddressStore l), Ord1 (AddressStore l), Show1 (AddressStore l), Eq1 (Cell l), Ord1 (Cell l), Show1 (Cell l), Traversable (Cell l), Applicative (Cell l)) => Address l where
  type AddressStore l :: * -> *
  type Cell l :: * -> *
  type Context l a (fs :: [* -> *]) :: Constraint
  type instance Context l a fs = (State (AddressStore l a) :< fs, MonadFail (Eff fs))

  deref :: Context l a fs => l a -> Eff fs a

  alloc :: Context l a fs => Name -> Eff fs (l a)

  assign :: Context l a fs => l a -> a -> Eff fs ()

  coerceAddress :: l a -> l b


newtype Precise a = Precise { unPrecise :: Int }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

allocPrecise :: AddressStore Precise a -> Precise a
allocPrecise = Precise . IntMap.size

instance Address Precise where
  type AddressStore Precise = IntMap.IntMap
  type Cell Precise = Identity

  deref = maybe uninitializedAddress pure <=< flip fmap get . IntMap.lookup . unPrecise

  alloc _ = fmap allocPrecise get

  assign = (modify .) . IntMap.insert . unPrecise

  coerceAddress = Precise . unPrecise


newtype Monovariant a = Monovariant { unMonovariant :: Name }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Address Monovariant where
  type AddressStore Monovariant = Store Monovariant
  type Cell Monovariant = []
  type Context Monovariant a fs = (Ord a, State (AddressStore Monovariant a) :< fs, Alternative (Eff fs), MonadFail (Eff fs))

  deref = maybe uninitializedAddress (asum . fmap pure) <=< flip fmap get . storeLookup

  alloc = pure . Monovariant

  assign = (modify .) . storeInsert

  coerceAddress = Monovariant . unMonovariant


addressEq :: Address l => l a -> l b -> Bool
addressEq = liftEq (const (const True))

addressCompare :: Address l => l a -> l b -> Ordering
addressCompare = liftCompare (const (const EQ))

addressShowsPrec :: Address l => Int -> l a -> ShowS
addressShowsPrec = liftShowsPrec hidesPrec hideList

addressShowList :: Address l => [l a] -> ShowS
addressShowList = liftShowList hidesPrec hideList


uninitializedAddress :: MonadFail m => m a
uninitializedAddress = fail "uninitialized address"


hidesPrec :: Int -> a -> ShowS
hidesPrec _ _ = id

hideList :: [a] -> ShowS
hideList _ = id


instance Address l => Foldable (Key l) where
  foldMap _ = mempty

instance Address l => Functor (Key l) where
  fmap f = Key . fmap f . unKey

instance Address l => Traversable (Key l) where
  traverse f = fmap Key . traverse f . unKey


instance Address l => Foldable (Store l) where
  foldMap = (. unStore) . foldMap . foldMap

instance Address l => Functor (Store l) where
  fmap f = Store . Map.mapKeys (Key . coerceAddress . unKey) . fmap (fmap f) . unStore

instance Address l => Traversable (Store l) where
  traverse f = fmap (Store . Map.mapKeys (Key . coerceAddress . unKey)) . traverse (traverse f) . unStore


instance Address l => Monoid (Store l a) where
  mempty = Store mempty
  mappend = (Store .) . (mappend `on` unStore)


instance Eq1 Precise where
  liftEq _ (Precise i1) (Precise i2) = i1 == i2

instance Eq1 Monovariant where
  liftEq _ (Monovariant n1) (Monovariant n2) = n1 == n2

instance Address l => Eq1 (Store l) where
  liftEq eq (Store m1) (Store m2) = liftEq2 (liftEq eq) (liftEq eq) m1 m2

instance Address l => Eq1 (Key l) where
  liftEq eq (Key a) (Key b) = liftEq eq a b

instance Address l => Eq (Key l a) where
  (==) = liftEq (const (const True))

instance Ord1 Precise where
  liftCompare _ (Precise i1) (Precise i2) = compare i1 i2

instance Ord1 Monovariant where
  liftCompare _ (Monovariant n1) (Monovariant n2) = compare n1 n2

instance Address l => Ord1 (Store l) where
  liftCompare compareA (Store m1) (Store m2) = liftCompare2 (liftCompare compareA) (liftCompare compareA) m1 m2

instance Address l => Ord1 (Key l) where
  liftCompare compareA (Key a) (Key b) = liftCompare compareA a b

instance Address l => Ord (Key l a) where
  compare = liftCompare (const (const EQ))

instance Show1 Precise where
  liftShowsPrec _ _ d (Precise i) = showsUnaryWith showsPrec "Precise" d i

instance Show1 Monovariant where
  liftShowsPrec _ _ d (Monovariant n) = showsUnaryWith showsPrec "Monovariant" d n

instance Address l => Show1 (Store l) where
  liftShowsPrec sp sl d (Store m) = showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Store" d m

instance Address l => Show (Key l a) where
  showsPrec d = showsUnaryWith (liftShowsPrec (const (const id)) (showListWith (const id))) "Key" d . unKey
