{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, DefaultSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
module Abstract.Eval where

import Abstract.Term
import Abstract.Store

import Data.Proxy
import Data.Union


-- Standard evaluator/interpreter
class Monad m => Eval v m syntax constr where
  evaluate :: (Term syntax -> m v) -> constr (Term syntax) -> m v


instance ( Monad m
         , Apply (Eval v m s) fs
         )
         => Eval v m s (Union fs) where
  evaluate ev = apply (Proxy :: Proxy (Eval v m s)) (evaluate ev)



class Monad m => MonadGC l a m where
  askRoots :: m (Set (Address l a))

  extraRoots :: Set (Address l a) -> m b -> m b


-- Collecting evaluator
class Monad m => EvalCollect l v m syntax constr where
  evalCollect :: (Term syntax -> m v)
              -> constr (Term syntax)
              -> m v
  default evalCollect :: (Eval v m syntax constr) => (Term syntax -> m v)
                      -> constr (Term syntax)
                      -> m v
  evalCollect = evaluate

instance ( Monad m
         , Apply (EvalCollect l v m s) fs
         )
         => EvalCollect l v m s (Union fs) where
  evalCollect ev = apply (Proxy :: Proxy (EvalCollect l v m s)) (evalCollect @l ev)
