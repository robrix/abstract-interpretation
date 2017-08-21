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
import Data.Coerce
import Data.Foldable (asum)
import Data.Function (on)
import Data.Functor.Classes
import Data.Functor.Identity
import qualified Data.IntMap as IntMap
import Data.Kind
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)

newtype Store l a = Store { unStore :: Map.Map (l a) (Cell l a) }

class (Eq1 l, Ord1 l, Show1 l, Eq1 (AddressStore l), Ord1 (AddressStore l), Show1 (AddressStore l), Eq1 (Cell l), Ord1 (Cell l), Show1 (Cell l)) => Address l where
  type AddressStore l :: * -> *
  type Cell l :: * -> *
  type Context l a (fs :: [* -> *]) :: Constraint
  type instance Context l a fs = (State (AddressStore l a) :< fs, MonadFail (Eff fs))

  deref :: Context l a fs => l a -> Eff fs a

  alloc :: Context l a fs => Name -> Eff fs (l a)

  assign :: Context l a fs => l a -> a -> Eff fs ()


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


newtype Monovariant a = Monovariant Name
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

newtype MonovariantStore a = MonovariantStore { unMonovariantStore :: Map.Map (Monovariant a) [a] }
  deriving (Eq, Ord, Show)

monovariantLookup :: Monovariant a -> MonovariantStore a -> Maybe [a]
monovariantLookup = (. unMonovariantStore) . Map.lookup

monovariantInsert :: Ord a => Monovariant a -> a -> MonovariantStore a -> MonovariantStore a
monovariantInsert = (((MonovariantStore .) . (. unMonovariantStore)) .) . (. pure) . Map.insertWith (<>)

instance Address Monovariant where
  type AddressStore Monovariant = MonovariantStore
  type Cell Monovariant = []
  type Context Monovariant a fs = (Ord a, State (AddressStore Monovariant a) :< fs, Alternative (Eff fs), MonadFail (Eff fs))

  deref = maybe uninitializedAddress (asum . fmap pure) <=< flip fmap get . monovariantLookup

  alloc = pure . Monovariant

  assign = (modify .) . monovariantInsert


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


instance Foldable MonovariantStore where
  foldMap = (. unMonovariantStore) . foldMap . foldMap


instance Functor MonovariantStore where
  fmap f = MonovariantStore . Map.mapKeys coerce . fmap (map f) . unMonovariantStore


instance Traversable MonovariantStore where
  traverse f = fmap (MonovariantStore . Map.mapKeys coerce) . traverse (traverse f) . unMonovariantStore


instance Monoid (MonovariantStore a) where
  mempty = MonovariantStore mempty
  mappend = (MonovariantStore .) . (mappend `on` unMonovariantStore)


instance Eq1 Precise where
  liftEq _ (Precise i1) (Precise i2) = i1 == i2

instance Eq1 Monovariant where
  liftEq _ (Monovariant n1) (Monovariant n2) = n1 == n2

instance Eq1 MonovariantStore where
  liftEq eq (MonovariantStore m1) (MonovariantStore m2) = liftEq2 addressEq (liftEq eq) m1 m2

instance Ord1 Precise where
  liftCompare _ (Precise i1) (Precise i2) = compare i1 i2

instance Ord1 Monovariant where
  liftCompare _ (Monovariant n1) (Monovariant n2) = compare n1 n2

instance Ord1 MonovariantStore where
  liftCompare compareA (MonovariantStore m1) (MonovariantStore m2) = liftCompare2 addressCompare (liftCompare compareA) m1 m2

instance Show1 Precise where
  liftShowsPrec _ _ d (Precise i) = showsUnaryWith showsPrec "Precise" d i

instance Show1 Monovariant where
  liftShowsPrec _ _ d (Monovariant n) = showsUnaryWith showsPrec "Monovariant" d n

instance Show1 MonovariantStore where
  liftShowsPrec sp sl d (MonovariantStore m) = showsUnaryWith (liftShowsPrec2 addressShowsPrec addressShowList (liftShowsPrec sp sl) (liftShowList sp sl)) "MonovariantStore" d m
