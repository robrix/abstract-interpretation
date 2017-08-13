{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect.Internal hiding (run)
import Data.Function (fix)
import qualified Data.Set as Set

type DeadCodeInterpreter l i = DeadCode i ': Interpreter l i


-- Dead code analysis

evalDead :: forall l i. (Monoid (Store l i), Ord i, AbstractStore l, Context l i (DeadCodeInterpreter l i), AbstractValue i (Eff (DeadCodeInterpreter l i))) => Term i -> (Either String (Val l i, Set.Set (Term i)), Store l i)
evalDead = run @(DeadCodeInterpreter l i) . runDead

runDead :: (Ord i, DeadCodeInterpreter l i :<: fs, AbstractStore l, Context l i fs, AbstractValue i (Eff fs)) => Term i -> Eff fs (Val l i)
runDead e0 = do
  put (subterms e0)
  fix (evDead ev) e0

evDead :: (Ord i, DeadCodeInterpreter l i :<: fs)
       => ((Term i -> Eff fs (Val l i)) -> Term i -> Eff fs (Val l i))
       -> (Term i -> Eff fs (Val l i))
       -> Term i
       -> Eff fs (Val l i)
evDead ev0 ev e = do
  modify (Set.delete e)
  ev0 ev e

get :: (DeadCode i :< e) => Eff e (Set.Set (Term i))
get = send Get

put :: (DeadCode i :< e) => Set.Set (Term i) -> Eff e ()
put s = send (Put s)

modify :: (DeadCode i :< e) => (Set.Set (Term i) -> Set.Set (Term i)) -> Eff e ()
modify f = fmap f get >>= put

data DeadCode i a where
  Get :: DeadCode i (Set.Set (Term i))
  Put :: !(Set.Set (Term i)) -> DeadCode i ()


instance Ord i => RunEffect (DeadCode i) where
  type Result (DeadCode i) a = (a, Set.Set (Term i))
  runEffect = relayState mempty ((pure .) . flip (,)) $ \ state eff yield -> case eff of
    Get -> yield state state
    Put state' -> yield state' ()
