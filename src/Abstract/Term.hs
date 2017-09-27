{-# LANGUAGE StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Abstract.Term where

import Data.Union

type Name = String

-- a - Prim
-- f - Syntax
newtype Term a f = In { out :: f (Term a f)}

-- Smart constructor helper for Term
inject :: (g :< fs) => g (Term a (Union fs)) -> Term a (Union fs)
inject = In . inj


-- Instances

deriving instance Eq (f (Term a f)) => Eq (Term a f)
deriving instance Ord (f (Term a f)) => Ord (Term a f)
deriving instance Show (f (Term a f)) => Show (Term a f)
