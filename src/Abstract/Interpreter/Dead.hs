{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Abstract.Number
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Effect
import Control.Monad.Effect.Internal hiding (run)
import Data.Function (fix)
import qualified Data.Set as Set

type DeadCodeInterpreter l a = DeadCode a ': Interpreter l a

type DeadSet a = Set.Set (Term a)

type DeadCodeResult l a = (Either String (Value l a, DeadSet a), AddressStore l (Value l a))


-- Dead code analysis

evalDead :: forall l a. (Monoid (AddressStore l (Value l a)), Ord a, Address l, Context l (Value l a) (DeadCodeInterpreter l a), AbstractNumber a (Eff (DeadCodeInterpreter l a))) => Term a -> DeadCodeResult l a
evalDead = run @(DeadCodeInterpreter l a) . runDead

runDead :: (Ord a, DeadCodeInterpreter l a :<: fs, Address l, Context l (Value l a) fs, AbstractNumber a (Eff fs)) => Term a -> Eff fs (Value l a)
runDead e0 = do
  put (subterms e0)
  fix (evDead ev) e0

evDead :: (Ord a, DeadCodeInterpreter l a :<: fs)
       => ((Term a -> Eff fs (Value l a)) -> Term a -> Eff fs (Value l a))
       -> (Term a -> Eff fs (Value l a))
       -> Term a
       -> Eff fs (Value l a)
evDead ev0 ev e = do
  modify (Set.delete e)
  ev0 ev e


get :: (DeadCode a :< fs) => Eff fs (DeadSet a)
get = send Get

put :: (DeadCode a :< fs) => DeadSet a -> Eff fs ()
put s = send (Put s)

modify :: (DeadCode a :< fs) => (DeadSet a -> DeadSet a) -> Eff fs ()
modify f = fmap f get >>= put


data DeadCode a r where
  Get :: DeadCode a (DeadSet a)
  Put :: !(DeadSet a) -> DeadCode a ()


instance Ord a => RunEffect (DeadCode a) r where
  type Result (DeadCode a) r = (r, DeadSet a)
  runEffect = relayState mempty ((pure .) . flip (,)) $ \ state eff yield -> case eff of
    Get -> yield state state
    Put state' -> yield state' ()
