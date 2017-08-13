{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, InstanceSigs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Abstract.Store where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Foldable (asum)
import Data.Functor.Classes
import qualified Data.IntMap as IntMap
import Data.Kind
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Semigroup

newtype Precise a = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

newtype Monovariant a = Monovariant String
  deriving (Eq, Ord, Show)

class (Eq1 l, Ord1 l, Show1 l) => Address l where
  type Store l a
  type Context l a (fs :: [* -> *]) :: Constraint
  type instance Context l a fs = (State (Store l a) :< fs)

  find :: Context l a fs => l a -> Eff fs a

  alloc :: Context l a fs => String -> Eff fs (l a)

  ext :: Context l a fs => l a -> a -> Eff fs ()

instance Address Precise where
  type Store Precise a = IntMap.IntMap a
  find = flip fmap get . flip (IntMap.!) . unPrecise

  alloc :: forall a fs. (State (Store Precise a) :< fs) => String -> Eff fs (Precise a)
  alloc _ = do
    s <- get
    return (Precise (length (s :: Store Precise a)))

  ext (Precise loc) val = modify (IntMap.insert loc val)

instance Address Monovariant where
  type Store Monovariant a = Map.Map (Monovariant a) (Set.Set a)
  type Context Monovariant a fs = (Ord a, State (Store Monovariant a) :< fs, Alternative (Eff fs))

  find :: forall a fs. Context Monovariant a fs => Monovariant a -> Eff fs a
  find loc = do
    store <- get
    asum (return <$> Set.toList ((store :: Store Monovariant a) Map.! loc))

  alloc x = pure (Monovariant x)

  ext loc val = modify (Map.insertWith (<>) loc (Set.singleton val))

addressEq :: Address l => l a -> l b -> Bool
addressEq = liftEq (const (const True))

addressCompare :: Address l => l a -> l b -> Ordering
addressCompare = liftCompare (const (const EQ))

addressShowsPrec :: Address l => Int -> l a -> ShowS
addressShowsPrec = liftShowsPrec hidesPrec hideList

addressShowList :: Address l => [l a] -> ShowS
addressShowList = liftShowList hidesPrec hideList


hidesPrec :: Int -> a -> ShowS
hidesPrec _ _ = id

hideList :: [a] -> ShowS
hideList _ = id

instance Eq1 Precise where
  liftEq _ (Precise i1) (Precise i2) = i1 == i2

instance Eq1 Monovariant where
  liftEq _ (Monovariant n1) (Monovariant n2) = n1 == n2

instance Ord1 Precise where
  liftCompare _ (Precise i1) (Precise i2) = compare i1 i2

instance Ord1 Monovariant where
  liftCompare _ (Monovariant n1) (Monovariant n2) = compare n1 n2

instance Show1 Precise where
  liftShowsPrec _ _ d (Precise i) = showsUnaryWith showsPrec "Precise" d i

instance Show1 Monovariant where
  liftShowsPrec _ _ d (Monovariant n) = showsUnaryWith showsPrec "Monovariant" d n
