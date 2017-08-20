{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Abstract.Number
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.State
import Data.Function (fix)
import qualified Data.Set as Set

type DeadCodeInterpreter l a = State (DeadSet a) ': Interpreter l a

type DeadSet a = Set.Set (Term a)

type DeadCodeResult l a = (Either String (Value l a, DeadSet a), AddressStore l (Value l a))


-- Dead code analysis

evalDead :: forall l a. (Monoid (AddressStore l (Value l a)), Ord a, Address l, Context l (Value l a) (DeadCodeInterpreter l a), AbstractNumber a (Eff (DeadCodeInterpreter l a))) => Term a -> DeadCodeResult l a
evalDead = run @(DeadCodeInterpreter l a) . runDead

runDead :: (Ord a, DeadCodeInterpreter l a :<: fs, Address l, Context l (Value l a) fs, AbstractNumber a (Eff fs)) => Eval l fs a
runDead e0 = do
  put (subterms e0)
  fix (evDead ev) e0

evDead :: (Ord a, DeadCodeInterpreter l a :<: fs)
       => (Eval l fs a -> Eval l fs a)
       -> Eval l fs a
       -> Eval l fs a
evDead ev0 ev e = do
  modify (Set.delete e)
  ev0 ev e
