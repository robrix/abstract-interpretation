{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Abs.Syntax where

import Data.Functor.Foldable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

data Syntax n a
  = Var n
  | Num Int
  | Op2 Op2 a a
  | App a a
  | Lam n a
  | Rec n a
  | If0 a a a
  deriving (Eq, Show)

type Term = Fix (Syntax String)

data Op2 = Plus | Minus | Times | DividedBy
  deriving (Eq, Show)

data Ref a = Ref

delta :: Monad m => Op2 -> Int -> Int -> m Int

type Environment = Map.Map String Loc
type Loc = Int
type Val = Int
type Store = IntMap.IntMap Loc
delta o = (return .) . case o of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  DividedBy -> div

type Interpreter = Eff '[State Store, Reader Environment, Failure]
