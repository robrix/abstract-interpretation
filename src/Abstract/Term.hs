{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Abstract.Term where

import Abstract.Set

import Data.Function
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Proxy
import Data.Union

type Name = String

newtype Term syntax = In { out :: syntax (Term syntax) }

type instance Base (Term syntax) = syntax

instance (Functor syntax) => Recursive (Term syntax) where
  project = out


class FreeVariables1 syntax where
  liftFreeVariables :: (a -> Set Name) -> syntax a -> Set Name
  default liftFreeVariables :: (Foldable syntax) => (a -> Set Name) -> syntax a -> Set Name
  liftFreeVariables = foldMap

class FreeVariables term where
  freeVariables :: term -> Set Name

instance (FreeVariables1 syntax, Functor syntax) => FreeVariables (Term syntax) where
  freeVariables = cata (liftFreeVariables id)

instance (Apply FreeVariables1 fs) => FreeVariables1 (Union fs) where
  liftFreeVariables f = apply (Proxy :: Proxy FreeVariables1) (liftFreeVariables f)


-- Smart constructor helper for Term
inject :: (g :< fs) => g (Term (Union fs)) -> Term (Union fs)
inject = In . inj

-- Instances

instance Eq1 syntax => Eq (Term syntax) where
  (==) = liftEq (==) `on` out

instance Ord1 syntax => Ord (Term syntax) where
  compare (In a) (In b) = liftCompare compare a b

instance Show1 syntax => Show (Term syntax) where
  showsPrec p = liftShowsPrec showsPrec showList p . out
