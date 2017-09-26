{-# LANGUAGE StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Abstract.Expr where

import Data.Union

newtype Expr f = In' (f (Expr f))
deriving instance Show (f (Expr f)) => Show (Expr f)

inject :: (g :< f) => g (Expr (Union f)) -> Expr (Union f)
inject = In' . inj
