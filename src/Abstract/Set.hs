{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Abstract.Set where

import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc

newtype Set a = Set { unSet :: Set.Set a }
  deriving (Eq, Eq1, Foldable, Monoid, Ord, Ord1, Pointed, Semigroup, Show, Show1)

member :: Ord a => a -> Set a -> Bool
member = (. unSet) . Set.member

delete :: Ord a => a -> Set a -> Set a
delete a = Set . Set.delete a . unSet


instance Pretty1 Set where
  liftPretty _ pl = pl . toList

instance Pretty a => Pretty (Set a) where
  pretty = liftPretty pretty prettyList
