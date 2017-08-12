{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abs.Syntax where

import Control.Monad.Effect as Effect
import Control.Monad.Effect.Failure
import qualified Control.Monad.Effect.Reader as Reader
import qualified Control.Monad.Effect.State as State
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Function ((&))
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

find :: State :< fs => Loc -> Eff fs Val
find = gets . flip (IntMap.!)

alloc :: State :< fs => String -> Eff fs Loc
alloc _ = do
  s <- get
  return (length (s :: Store))

ext :: State :< fs => Loc -> Val -> Eff fs ()
ext loc val = modify (IntMap.insert loc val)


type Environment = Map.Map String Loc
type Loc = Int
type Val = Int
type Store = IntMap.IntMap Loc

ev :: (Reader :< fs, State :< fs) => (Term -> Eff fs Val) -> Term -> Eff fs Val
ev ev term = case unfix term of
  Num n -> return n
  Var x -> do
    p <- ask
    find (p Map.! x)
  If0 c t e -> do
    z <- ev c
    ev (if z == 0 then t else e)
  Op2 o a b -> do
    va <- ev a
    vb <- ev b
    delta o va vb
  Rec f e -> do
    p <- ask
    a <- alloc f
    let p' = Map.insert f a p
    v <- local (const p') (ev e)
    ext a v
    return v
  Lam x e0 -> do
    p <- ask
    return (Fix (Lam x e0), p)
  App e0 e1 -> do
    (Fix (Lam x e2), p) <- ev e0
    v1 <- ev e1
    a <- alloc x
    ext a v1
    local (const (Map.insert x a p)) (ev e2)


delta :: Monad m => Op2 -> Val -> Val -> m Val
delta o = (return .) . case o of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  DividedBy -> div

type Interpreter = Eff '[State, Reader, Failure]
type State = State.State Store
type Reader = Reader.Reader Environment

run :: Interpreter a -> Either String a
run f = State.runState f IntMap.empty
      & flip Reader.runReader Map.empty
      & runFailure
      & Effect.run
      & fmap fst

instance State :< fs => MonadState Store (Eff fs) where
  get = State.get
  put = State.put

instance Reader :< fs => MonadReader Environment (Eff fs) where
  ask = Reader.ask
  local = Reader.local
