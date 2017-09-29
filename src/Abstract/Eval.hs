{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
module Abstract.Eval where

import Abstract.Term
import Abstract.Type
import Abstract.Value
import Abstract.Store
import Abstract.Environment

import Control.Applicative
import Control.Monad.Fail
import Data.Proxy
import Data.Semigroup
import Data.Union


-- Standard evaluator/interpreter
class Monad m => Eval v m syntax constr where
  evaluate :: (Term syntax -> m v) -> constr (Term syntax) -> m v


instance ( Apply (Eval (Value s l) m s) fs
         , Monad m
         , MonadFail m
         , MonadAddress l m
         , MonadStore l (Value s l) m
         , MonadEnv l (Value s l) m
         , Semigroup (Cell l (Value s l))
         )
         => Eval (Value s l) m s (Union fs) where
  evaluate ev = apply (Proxy :: Proxy (Eval (Value s l) m s)) (evaluate ev)

instance ( Apply (Eval Type m s) fs
         , Alternative m
         , MonadFresh m
         , MonadFail m
         , MonadStore Monovariant Type m
         , MonadEnv Monovariant Type m
         , Semigroup (Cell Monovariant Type)
         )
         => Eval Type m s (Union fs) where
  evaluate ev = apply (Proxy :: Proxy (Eval Type m s)) (evaluate ev)


-- Collecting evaluator
class Monad m => EvalCollect v m syntax where
  evalCollect :: ((Term syntax -> m v) -> (Term syntax -> m v))
              -> (Term syntax -> m v)
              -> Term syntax
              -> m v
  default evalCollect :: ((Term syntax -> m v) -> (Term syntax -> m v))
                      -> (Term syntax -> m v)
                      -> Term syntax
                      -> m v
  evalCollect ev0 = ev0
