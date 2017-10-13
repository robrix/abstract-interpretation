{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Abstract.Term
import Abstract.Eval

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Functor.Foldable
import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup


type DeadCodeInterpreter l t v = State (Dead t) ': Interpreter l v

type DeadCodeResult l t v = Final (DeadCodeInterpreter l t v) v


newtype Dead a = Dead { unDead :: Set a }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)


class Monad m => MonadDead t m where
  killAll :: Dead t -> m ()
  revive :: Ord t => t -> m ()

instance (State (Dead t) :< fs) => MonadDead t (Eff fs) where
  killAll = put
  revive = modify . (Dead .) . (. unDead) . delete


subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Set a
subterms term = para (foldMap (uncurry ((<>) . point))) term <> point term


-- Dead code analysis
-- Example:
--    evalDead @Precise @(Value Syntax Precise) @Syntax (if' true (Abstract.Syntax.int 1) (Abstract.Syntax.int 2))
evalDead :: forall l v s
         . ( Ord v
           , Ord1 s
           , Recursive (Term s)
           , Foldable (Base (Term s))
           , Eval v (Eff (DeadCodeInterpreter l (Term s) v)) s s
           , MonadAddress l (Eff (DeadCodeInterpreter l (Term s) v))
           , MonadPrim v (Eff (DeadCodeInterpreter l (Term s) v))
           , Semigroup (Cell l v)
           )
         => Term s
         -> DeadCodeResult l (Term s) v
evalDead e0 = run @(DeadCodeInterpreter l (Term s) v) $ do
  killAll (Dead (subterms e0))
  fix (evDead ev) e0

evDead :: (Ord t, MonadDead t m)
       => (Eval' t (m v) -> Eval' t (m v))
       -> Eval' t (m v)
       -> Eval' t (m v)
evDead ev0 ev e = do
  revive e
  ev0 ev e
