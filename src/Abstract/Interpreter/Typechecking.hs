{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter.Typechecking where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Type
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Data.Function (fix)
import Data.Proxy

type TypecheckingInterpreter l v = Interpreter l v

type TypecheckingResult l v = (Either String v, Store l v)

evalCheck :: forall l a. (Ord a, Address l, Context l (Value l (Term a) a) (TypecheckingInterpreter l (Value l (Term a) a)), PrimitiveOperations a (Eff (TypecheckingInterpreter l (Value l (Term a) a)))) => Term a -> TypecheckingResult l (Value l (Term a) a)
evalCheck = run @(TypecheckingInterpreter l (Value l (Term a) a)) . runCheck (Proxy :: Proxy l) ev

runCheck :: TypecheckingInterpreter l v :<: fs => proxy l -> (Eval t fs v -> Eval t fs v) -> Eval t fs v
runCheck proxy ev e0 = fix (evCheck proxy ev) e0

evCheck :: TypecheckingInterpreter l v :<: fs
        => proxy l
        -> (Eval t fs v -> Eval t fs v)
        -> Eval t fs v
        -> Eval t fs v
evCheck _ ev0 ev e = ev0 ev e
