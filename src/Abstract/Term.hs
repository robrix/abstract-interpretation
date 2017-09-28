{-# LANGUAGE TypeOperators #-}

module Abstract.Term where

import Data.Union
import Data.Functor.Classes
import Data.Function

type Name = String

-- a - Prim
-- f - Syntax
newtype Term a f = In { out :: f (Term a f)}

-- Smart constructor helper for Term
inject :: (g :< fs) => g (Term a (Union fs)) -> Term a (Union fs)
inject = In . inj


-- Instances

instance Eq1 syntax => Eq (Term a syntax) where
  (==) = liftEq (==) `on` out

instance Ord1 syntax => Ord (Term a syntax) where
  compare (In a) (In b) = liftCompare compare a b

instance Show1 syntax => Show (Term a syntax) where
  showsPrec p = liftShowsPrec showsPrec showList p . out
