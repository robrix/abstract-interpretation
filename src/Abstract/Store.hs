{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, InstanceSigs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Abstract.Store
( Precise(..)
, Monovariant(..)
, Address(..)
, addressEq
, addressCompare
, addressShowsPrec
, addressShowList
) where

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
import Data.Foldable (asum)
import Data.Functor.Classes
import qualified Data.IntMap as IntMap
import Data.Kind
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Semigroup
import Prelude hiding (fail)

class (Eq1 l, Ord1 l, Show1 l) => Address l where
  type AddressStore l a
  type Context l a (fs :: [* -> *]) :: Constraint
  type instance Context l a fs = (State (AddressStore l a) :< fs, MonadFail (Eff fs))

  find :: Context l a fs => l a -> Eff fs a

  alloc :: Context l a fs => String -> Eff fs (l a)

  ext :: Context l a fs => l a -> a -> Eff fs ()

  liftEqStore :: proxy l -> (a -> b -> Bool) -> AddressStore l a -> AddressStore l b -> Bool
  liftCompareStore :: proxy l -> (a -> b -> Ordering) -> AddressStore l a -> AddressStore l b -> Ordering
  liftShowsPrecStore :: proxy l -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> AddressStore l a -> ShowS
  liftShowListStore :: proxy l -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> [AddressStore l a] -> ShowS


newtype Precise a = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

allocPrecise :: AddressStore Precise a -> Precise a
allocPrecise = Precise . IntMap.size

instance Address Precise where
  type AddressStore Precise a = IntMap.IntMap a

  find = maybe uninitializedAddress pure <=< flip fmap get . IntMap.lookup . unPrecise

  alloc _ = fmap allocPrecise get

  ext = (modify .) . IntMap.insert . unPrecise

  liftEqStore _ = liftEq
  liftCompareStore _ = liftCompare
  liftShowsPrecStore _ = liftShowsPrec
  liftShowListStore _ = liftShowList


newtype Monovariant a = Monovariant String
  deriving (Eq, Ord, Show)

newtype MonovariantStore a = MonovariantStore { unMonovariantStore :: Map.Map (Monovariant a) (Set.Set a) }
  deriving (Eq, Ord, Show)

monovariantLookup :: Monovariant a -> MonovariantStore a -> Maybe (Set.Set a)
monovariantLookup = (. unMonovariantStore) . Map.lookup

monovariantInsert :: Ord a => Monovariant a -> a -> MonovariantStore a -> MonovariantStore a
monovariantInsert = (((MonovariantStore .) . (. unMonovariantStore)) .) . (. Set.singleton) . Map.insertWith (<>)

instance Address Monovariant where
  type AddressStore Monovariant a = MonovariantStore a
  type Context Monovariant a fs = (Ord a, State (AddressStore Monovariant a) :< fs, Alternative (Eff fs), MonadFail (Eff fs))

  find = maybe uninitializedAddress (asum . fmap pure . Set.toList) <=< flip fmap get . monovariantLookup

  alloc = pure . Monovariant

  ext = (modify .) . monovariantInsert

  liftEqStore _ = liftEq
  liftCompareStore _ = liftCompare
  liftShowsPrecStore _ = liftShowsPrec
  liftShowListStore _ = liftShowList

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
