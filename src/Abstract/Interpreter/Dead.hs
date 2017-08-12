{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.State
import Data.Function (fix)
import qualified Data.Set as Set

type DeadCodeInterpreter i = State (Set.Set (Term i)) ': Interpreter i


-- Dead code analysis

evalDead :: forall i. (Ord i, AbstractValue i (Eff (DeadCodeInterpreter i))) => Term i -> (Either String (Val i, Set.Set (Term i)), Store (Val i))
evalDead = run @(DeadCodeInterpreter i) . runDead

runDead :: (Ord i, DeadCodeInterpreter i :<: fs, AbstractValue i (Eff fs)) => Term i -> Eff fs (Val i)
runDead e0 = do
  put (subexps e0)
  fix (evDead ev) e0

evDead :: (Ord i, DeadCodeInterpreter i :<: fs)
       => ((Term i -> Eff fs (Val i)) -> Term i -> Eff fs (Val i))
       -> (Term i -> Eff fs (Val i))
       -> Term i
       -> Eff fs (Val i)
evDead ev0 ev e = do
  modify (Set.delete e)
  ev0 ev e
