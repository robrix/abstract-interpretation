{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter where

import Abstract.Environment
import Abstract.Primitive
import Abstract.Store
import Abstract.Term
import Abstract.Type
import Abstract.Eval

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Semigroup
import Prelude hiding (fail)


type Interpreter l v = '[Fresh, Fail, NonDetEff, State (Store l v), Reader (Environment l v)]

type MonadInterpreter l v m = (MonadEnv l v m, MonadStore l v m, MonadFail m)

type EvalResult l v = Final (Interpreter l v) v

type Eval' t m = t -> m

-- Evaluate an expression.
-- Example:
--    eval @Precise @(Value Syntax Precise) @Syntax (makeLam "x" (var "x") # true)
eval :: forall l v s
     . ( Ord v
       , Eval v (Eff (Interpreter l v)) s s
       , MonadAddress l (Eff (Interpreter l v))
       , MonadPrim v (Eff (Interpreter l v))
       , Semigroup (Cell l v))
     => Term s
     -> EvalResult l v
eval = run @(Interpreter l v) . fix ev

ev :: (Eval v m syntax syntax) => (Term syntax -> m v) -> Term syntax -> m v
ev ev = evaluate ev . out
