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

type DeadCodeInterpreter f i = State (Set.Set (Term i)) ': Interpreter f i


-- Dead code analysis

evalDead :: forall f i. (Ord i, AbstractValue i (Eff (DeadCodeInterpreter f i))) => AbstractStore f (DeadCodeInterpreter f i) i -> Term i -> (Either String (Val i, Set.Set (Term i)), Store f i)
evalDead store = run @(DeadCodeInterpreter f i) . runDead store

runDead :: (Ord i, DeadCodeInterpreter f i :<: fs, AbstractValue i (Eff fs)) => AbstractStore f fs i -> Term i -> Eff fs (Val i)
runDead store e0 = do
  put (subterms e0)
  fix (evDead store (ev store)) e0

evDead :: (Ord i, DeadCodeInterpreter f i :<: fs)
       => AbstractStore f fs i
       -> ((Term i -> Eff fs (Val i)) -> Term i -> Eff fs (Val i))
       -> (Term i -> Eff fs (Val i))
       -> Term i
       -> Eff fs (Val i)
evDead _ ev0 ev e = do
  modify (Set.delete e)
  ev0 ev e
