{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter.Typechecking where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Type
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Amb
import Data.Function (fix)

type TypecheckingInterpreter l = Amb ': Interpreter l Type

type TypecheckingResult l = (Either String [Type], Store l Type)

evalCheck :: forall l
          .  (Address l, Context l Type (Eff (TypecheckingInterpreter l)))
          => Eval (Term Prim) (TypecheckingResult l)
evalCheck = run @(TypecheckingInterpreter l) . runCheck @l (ev @l)

runCheck :: forall l t fs. TypecheckingInterpreter l :<: fs => (Eval t (Eff fs Type) -> Eval t (Eff fs Type)) -> Eval t (Eff fs Type)
runCheck ev e0 = fix (evCheck @l ev) e0

evCheck :: TypecheckingInterpreter l :<: fs
        => (Eval t (Eff fs Type) -> Eval t (Eff fs Type))
        -> Eval t (Eff fs Type)
        -> Eval t (Eff fs Type)
evCheck ev0 ev e = ev0 ev e
