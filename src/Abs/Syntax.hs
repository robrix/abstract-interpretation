{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abs.Syntax where

import Control.Monad.Fail
import Control.Monad.Effect as Effect hiding (run)
import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Failure
import qualified Control.Monad.Effect.Reader as Reader
import qualified Control.Monad.Effect.State as State
import qualified Control.Monad.Effect.Writer as Writer
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Function ((&), fix)
import Data.Functor.Classes
import Data.Functor.Foldable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Prelude hiding (fail)

data Syntax n a
  = Var n
  | Num Int
  | Op1 Op1 a
  | Op2 Op2 a a
  | App a a
  | Lam n a
  | Rec n a
  | If0 a a a
  deriving (Eq, Show)

type Term = Fix (Syntax String)

data Op1 = Negate | Abs | Signum
  deriving (Eq, Show)

data Op2 = Plus | Minus | Times | DividedBy
  deriving (Eq, Show)

find :: State Store :< fs => Loc -> Eff fs Val
find = gets . flip (IntMap.!)

alloc :: State Store :< fs => String -> Eff fs Loc
alloc _ = do
  s <- get
  return (length (s :: Store))

ext :: State Store :< fs => Loc -> Val -> Eff fs ()
ext loc val = modify (IntMap.insert loc val)


type Environment = Map.Map String Loc
type Loc = Int
data Val = I Int | L (Term, Environment)
  deriving (Eq, Show)
type Store = IntMap.IntMap Val

ev :: Interpreter :<: fs => (Term -> Eff fs Val) -> Term -> Eff fs Val
ev ev term = case unfix term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    find (p Map.! x)
  If0 c t e -> do
    I z <- ev c
    ev (if z == 0 then t else e)
  Op1 o a -> do
    va <- ev a
    delta1 o va
  Op2 o a b -> do
    va <- ev a
    vb <- ev b
    delta2 o va vb
  Rec f e -> do
    p <- ask
    a <- alloc f
    let p' = Map.insert f a p
    v <- local (const p') (ev e)
    ext a v
    return v
  Lam x e0 -> do
    p <- ask
    return (L (Fix (Lam x e0), p))
  App e0 e1 -> do
    (L (Fix (Lam x e2), p)) <- ev e0
    v1 <- ev e1
    a <- alloc x
    ext a v1
    local (const (Map.insert x a p)) (ev e2)

evalTell :: Term -> Either String (Val, Trace)
evalTell = run . Writer.runWriter . fix (evTell ev)

evTell :: TracingInterpreter :<: fs => ((Term -> Eff fs Val) -> Term -> Eff fs Val) -> (Term -> Eff fs Val) -> Term -> Eff fs Val
evTell ev0 ev e = do
  env <- ask
  store <- get
  Writer.tell [(e, env, store)]
  ev0 ev e


delta1 :: Monad m => Op1 -> Val -> m Val
delta1 o = \ (I a) -> return . I $ case o of
  Negate -> negate a
  Abs -> abs a
  Signum -> signum a

delta2 :: MonadFail m => Op2 -> Val -> Val -> m Val
delta2 o = \ (I a) (I b) -> case o of
  Plus -> return . I $ a + b
  Minus -> return . I $ a - b
  Times -> return . I $ a * b
  DividedBy -> if b == 0 then
      fail "division by zero"
    else
      return . I $ a `div` b

type Interpreter = '[State Store, Reader, Failure]
type State = State.State
type Reader = Reader.Reader Environment
type Writer = Writer.Writer
type Trace = [(Term, Environment, Store)]
type TracingInterpreter = Writer Trace ': Interpreter

run :: Eff Interpreter a -> Either String a
run f = State.runState f IntMap.empty
      & flip Reader.runReader Map.empty
      & runFailure
      & Effect.run
      & fmap fst

instance State Store :< fs => MonadState Store (Eff fs) where
  get = State.get
  put = State.put

instance Reader :< fs => MonadReader Environment (Eff fs) where
  ask = Reader.ask
  local = Reader.local

instance Eq2 Syntax where
  liftEq2 eqN eqA s1 s2 = case (s1, s2) of
    (Var n1, Var n2) -> eqN n1 n2
    (Num v1, Num v2) -> v1 == v2
    (Op2 o1 a1 b1, Op2 o2 a2 b2) -> o1 == o2 && eqA a1 a2 && eqA b1 b2
    (App a1 b1, App a2 b2) -> eqA a1 a2 && eqA b1 b2
    (Lam n1 a1, Lam n2 a2) -> eqN n1 n2 && eqA a1 a2
    (Rec n1 a1, Rec n2 a2) -> eqN n1 n2 && eqA a1 a2
    (If0 c1 t1 e1, If0 c2 t2 e2) -> eqA c1 c2 && eqA t1 t2 && eqA e1 e2
    _ -> False

instance Eq n => Eq1 (Syntax n) where
  liftEq = liftEq2 (==)

instance Show2 Syntax where
  liftShowsPrec2 spN _ spA _ d s = case s of
    Var n -> showsUnaryWith spN "Var" d n
    Num v -> showsUnaryWith showsPrec "Num" d v
    Op1 o a -> showsBinaryWith showsPrec spA "Op1" d o a
    Op2 o a b -> showsTernaryWith showsPrec spA spA "Op2" d o a b
    App a b -> showsBinaryWith spA spA "App" d a b
    Lam n a -> showsBinaryWith spN spA "Lam" d n a
    Rec n a -> showsBinaryWith spN spA "Rec" d n a
    If0 c t e -> showsTernaryWith spA spA spA "If0" d c t e
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance Show n => Show1 (Syntax n) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Num Term where
  fromInteger = Fix . Num . fromInteger

  signum = Fix . Op1 Signum
  abs = Fix . Op1 Abs
  negate = Fix . Op1 Negate
  (+) = (Fix .) . Op2 Plus
  (*) = (Fix .) . Op2 Times
