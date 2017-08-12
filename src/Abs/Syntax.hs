{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abs.Syntax where

import Control.Monad.Effect.State hiding (get, modify, put)
import qualified Control.Monad.Effect.State as State
import Control.Monad.State.Class
import Data.Functor.Foldable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

data Syntax n a
  = Var n
  | Num Val
  | Op2 Op2 a a
  | App a a
  | Lam n a
  | Rec n a
  | If0 a a a
  deriving (Eq, Show)

type Term = Fix (Syntax String)

data Op2 = Plus | Minus | Times | DividedBy
  deriving (Eq, Show)



type Environment = Map.Map String Loc
type Loc = Int
type Val = Int
type Store = IntMap.IntMap Loc


delta :: Monad m => Op2 -> Val -> Val -> m Val
delta o = (return .) . case o of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  DividedBy -> div

type Interpreter = Eff '[State Store, Reader Environment, Failure]

instance State Store :< fs => MonadState Store (Eff fs) where
  get = State.get
  put = State.put
