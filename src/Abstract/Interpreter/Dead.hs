{-# LANGUAGE DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
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

type DeadCodeInterpreter l a = State (Dead a) ': Interpreter l (Term a) a

newtype Dead a = Dead { unDead :: Set.Set (Term a) }
  deriving (Eq, Foldable, Monoid, Ord, Show)

revive :: Ord a => Term a -> Dead a -> Dead a
revive = (Dead .) . (. unDead) . Set.delete

type DeadCodeResult l a = (Either String (Value l (Term a) a, Dead a), Store l (Value l (Term a) a))


-- Dead code analysis

evalDead :: forall l a. (Monoid (Store l (Value l (Term a) a)), Ord a, Address l, Context l (Value l (Term a) a) (DeadCodeInterpreter l a), AbstractNumber a (Eff (DeadCodeInterpreter l a))) => Term a -> DeadCodeResult l a
evalDead = run @(DeadCodeInterpreter l a) . runDead

runDead :: (Ord a, DeadCodeInterpreter l a :<: fs, Address l, Context l (Value l (Term a) a) fs, AbstractNumber a (Eff fs)) => Eval (Term a) fs (Value l (Term a) a)
runDead e0 = do
  put (Dead (subterms e0))
  fix (evDead ev) e0

evDead :: (Ord a, DeadCodeInterpreter l a :<: fs)
       => (Eval (Term a) fs (Value l (Term a) a) -> Eval (Term a) fs (Value l (Term a) a))
       -> Eval (Term a) fs (Value l (Term a) a)
       -> Eval (Term a) fs (Value l (Term a) a)
evDead ev0 ev e = do
  modify (revive e)
  ev0 ev e
