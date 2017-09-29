{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Abstract.Term where

import Data.Function
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Union

type Name = String

newtype Term syntax = In { out :: syntax (Term syntax) }

type instance Base (Term syntax) = syntax

instance (Functor syntax) => Recursive (Term syntax) where
  project = out


class Monad m => Eval v m syntax constr where
  evaluate :: (Term syntax -> m v) -> constr (Term syntax) -> m v

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
