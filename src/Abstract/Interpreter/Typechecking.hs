{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter.Typechecking where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Data.Function (fix)

type TypecheckingInterpreter l v = Interpreter l v

type TypecheckingResult l v = (Either String v, Store l v)

evalCheck :: forall l v a
          .  (Ord a, Address l, Context l v (Eff (TypecheckingInterpreter l v)), AbstractValue l v Term a, PrimitiveOperations v (Eff (TypecheckingInterpreter l v)))
          => Eval (Term a) (TypecheckingResult l v)
evalCheck = run @(TypecheckingInterpreter l v) . runCheck @l (ev @l)

runCheck :: forall l t v fs. TypecheckingInterpreter l v :<: fs => (Eval t (Eff fs v) -> Eval t (Eff fs v)) -> Eval t (Eff fs v)
runCheck ev e0 = fix (evCheck @l ev) e0

evCheck :: TypecheckingInterpreter l v :<: fs
        => (Eval t (Eff fs v) -> Eval t (Eff fs v))
        -> Eval t (Eff fs v)
        -> Eval t (Eff fs v)
evCheck ev0 ev e = ev0 ev e
