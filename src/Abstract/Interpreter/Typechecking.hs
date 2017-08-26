{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter.Typechecking where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Abstract.Syntax
import Abstract.Type
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.NonDetEff
import Data.Function (fix)

type TypecheckingInterpreter l = NonDetEff ': Interpreter l Type

type TypecheckingResult l = Final (TypecheckingInterpreter l) Type

evalCheck :: forall l
          .  (Ord l, MonadAddress l Type (Eff (TypecheckingInterpreter l)))
          => Eval (Term Prim) (TypecheckingResult l)
evalCheck = run @(TypecheckingInterpreter l) . runCheck (ev @l)

runCheck :: (Eval t (m Type) -> Eval t (m Type))
         -> Eval t (m Type)
runCheck ev e0 = fix (evCheck ev) e0

evCheck :: (Eval t (m Type) -> Eval t (m Type))
        -> Eval t (m Type)
        -> Eval t (m Type)
evCheck ev0 ev e = ev0 ev e
