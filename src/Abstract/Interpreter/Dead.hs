{-# LANGUAGE DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Abstract.Number
import Abstract.Store
import Abstract.Syntax hiding (subterms)
import Abstract.Value
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Functor.Foldable
import Data.Semigroup
import qualified Data.Set as Set

type DeadCodeInterpreter l t v = State (Dead t) ': Interpreter l v

newtype Dead a = Dead { unDead :: Set.Set a }
  deriving (Eq, Foldable, Monoid, Ord, Show)

revive :: Ord a => a -> Dead a -> Dead a
revive = (Dead .) . (. unDead) . Set.delete

type DeadCodeResult l a = (Either String (Value l (Term a) a, Dead (Term a)), Store l (Value l (Term a) a))


subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Set.Set a
subterms term = para (foldMap (uncurry ((<>) . Set.singleton))) term <> Set.singleton term


-- Dead code analysis

evalDead :: forall l a. (Ord a, Address l, Context l (Value l (Term a) a) (DeadCodeInterpreter l (Term a) (Value l (Term a) a)), AbstractNumber a (Eff (DeadCodeInterpreter l (Term a) (Value l (Term a) a)))) => Term a -> DeadCodeResult l a
evalDead = run @(DeadCodeInterpreter l (Term a) (Value l (Term a) a)) . runDead ev

runDead :: (Ord a, Ord t, Recursive t, Foldable (Base t), DeadCodeInterpreter l t (Value l t a) :<: fs, Address l, Context l (Value l t a) fs, AbstractNumber a (Eff fs)) => (Eval t fs (Value l t a) -> Eval t fs (Value l t a)) -> Eval t fs (Value l t a)
runDead ev e0 = do
  put (Dead (subterms e0))
  fix (evDead ev) e0

evDead :: (Ord a, Ord t, DeadCodeInterpreter l t (Value l t a) :<: fs)
       => (Eval t fs (Value l t a) -> Eval t fs (Value l t a))
       -> Eval t fs (Value l t a)
       -> Eval t fs (Value l t a)
evDead ev0 ev e = do
  modify (revive e)
  ev0 ev e
