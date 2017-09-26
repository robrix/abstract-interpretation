{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Abstract.Eval where

import Data.Proxy (Proxy(..))
import Data.Union
import Abstract.Primitive
import Abstract.Expr
-- import Abstract.Value

data Value' f l
  = Literal Prim
  | Closure Name f (Environment l (Value' f l))

class Eval f where
  eval :: Eval g => Environment l a -> f (Expr g) -> Value' (Expr g) l

instance (Apply Eval fs, Apply Functor fs) => Eval (Union fs) where
  eval env = apply (Proxy :: Proxy Eval) (eval env)


eval'  :: Eval f => Expr f -> Value' (Expr f) l
eval' = undefined
