{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abs.Syntax where

import Abs.Value
import Control.Monad.Fail
import Control.Monad.Effect as Effect hiding (run)
import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Failure
import Control.Monad.Effect.Reader as Reader
import Control.Monad.Effect.State as State
import qualified Control.Monad.Effect.Writer as Writer
import Data.Bifunctor
import Data.Function ((&), fix)
import Data.Functor.Classes
import Data.Functor.Foldable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Semigroup
import qualified Data.Set as Set
import GHC.Exts (IsList(..))
import Prelude hiding (fail)

data Syntax i n a
  = Var n
  | Num i
  | Op1 Op1 a
  | Op2 Op2 a a
  | App a a
  | Lam n a
  | Rec n a
  | If0 a a a
  deriving (Eq, Ord, Show)

type Term i = Fix (Syntax i String)

var :: String -> Term i
var = Fix . Var

infixl 0 #
(#) :: Term i -> Term i -> Term i
(#) = (Fix .) . App

lam :: String -> (Term i -> Term i) -> Term i
lam s f = makeLam s (f (var s))

makeLam :: String -> Term i -> Term i
makeLam = (Fix .) . Lam

rec :: String -> Term i -> Term i
rec = (Fix .) . Rec

if0 :: Term i -> Term i -> Term i -> Term i
if0 c t e = Fix (If0 c t e)

subexps :: Ord i => Term i -> Set.Set (Term i)
subexps = para $ \ s -> case s of
  Op1 _ a -> Set.singleton (fst a) <> snd a
  Op2 _ a b -> Set.singleton (fst a) <> snd a <> Set.singleton (fst b) <> snd b
  App a b -> Set.singleton (fst a) <> snd a <> Set.singleton (fst b) <> snd b
  Lam _ a -> Set.singleton (fst a) <> snd a
  Rec _ a -> Set.singleton (fst a) <> snd a
  If0 c t e -> foldMap (Set.singleton . fst) [c, t, e] <> foldMap snd [c, t, e]
  _ -> Set.empty

find :: (State (Store i) :< fs) => Loc i -> Eff fs (Val i)
find = gets . flip (IntMap.!) . unLoc

gets :: (State a :< fs) => (a -> b) -> Eff fs b
gets = flip fmap get

alloc :: forall i fs . (State (Store i) :< fs) => String -> Eff fs (Loc i)
alloc _ = do
  s <- get
  return (Loc (length (s :: Store i)))

ext :: (State (Store i) :< fs) => Loc i -> Val i -> Eff fs ()
ext (Loc loc) val = modify (IntMap.insert loc val)


type Environment i = Map.Map String (Loc i)
newtype Loc i = Loc { unLoc :: Int }
  deriving (Eq, Ord, Show)
data Val i = I i | L (Term i, Environment i)
  deriving (Eq, Ord, Show)
type Store i = IntMap.IntMap (Val i)


-- Evaluation

eval :: forall i. AbstractValue i (Eff (Interpreter i)) => Term i -> Either String (Val i)
eval = run (undefined :: proxy i) . fix ev

ev :: (AbstractValue i (Eff fs), Interpreter i :<: fs)
   => (Term i -> Eff fs (Val i))
   -> Term i
   -> Eff fs (Val i)
ev ev term = case unfix term of
  Num n -> return (I n)
  Var x -> do
    p <- ask
    find (p Map.! x)
  If0 c t e -> do
    v <- ev c
    z <- isZero v
    ev (if z then t else e)
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


-- Tracing and reachable state analyses

evalTrace :: forall i. (AbstractValue i (Eff (TracingInterpreter i []))) => Term i -> Either String (Val i, Trace i [])
evalTrace = run (undefined :: proxy1 i) . Writer.runWriter . fix (evTell (undefined :: proxy2 []) ev)

evalReach :: forall i fs proxy0. (Ord i, AbstractValue i (Eff (TracingInterpreter i Set.Set))) => proxy0 fs -> Term i -> Either String (Val i, Trace i Set.Set)
evalReach _ = run (undefined :: proxy1 i) . Writer.runWriter . fix (evTell (undefined :: proxy2 Set.Set) ev)

evTell :: forall proxy i f fs . (TracingInterpreter i f :<: fs, IsList (Trace i f), Item (Trace i f) ~ TraceEntry i)
       => proxy f
       -> ((Term i -> Eff fs (Val i)) -> Term i -> Eff fs (Val i))
       -> (Term i -> Eff fs (Val i))
       -> Term i
       -> Eff fs (Val i)
evTell _ ev0 ev e = do
  env <- ask
  store <- get
  Writer.tell (fromList [(e, env, store)] :: Trace i f)
  ev0 ev e


-- Dead code analysis

evalDead :: forall i. (Ord i, AbstractValue i (Eff (DeadCodeInterpreter i))) => Term i -> Either String (Val i, Set.Set (Term i))
evalDead = run (undefined :: proxy1 i) . flip State.runState Set.empty . evalDead' (fix (evDead ev))
  where evalDead' eval e0 = do
          put (subexps e0)
          eval e0

evDead :: (Ord i, DeadCodeInterpreter i :<: fs)
       => ((Term i -> Eff fs (Val i)) -> Term i -> Eff fs (Val i))
       -> (Term i -> Eff fs (Val i))
       -> Term i
       -> Eff fs (Val i)
evDead ev0 ev e = do
  modify (Set.delete e)
  ev0 ev e


instance (MonadFail m, AbstractValue i m) => AbstractValue (Val i) m where
  delta1 o (I a) = fmap I (delta1 o a)
  delta1 _ _ = fail "non-numeric value"

  delta2 o (I a) (I b) = fmap I (delta2 o a b)
  delta2 _ _ _ = fail "non-numeric value"

  isZero (I a) = isZero a
  isZero _ = fail "non-numeric value"

type Interpreter i = '[State (Store i), Reader (Environment i), Failure]
type Writer = Writer.Writer
type Trace i f = f (TraceEntry i)
type TraceEntry i = (Term i, Environment i, Store i)
type TracingInterpreter i f = Writer (Trace i f) ': Interpreter i
type ReachableStateInterpreter i = Writer (Trace i Set.Set) ': Interpreter i
type DeadCodeInterpreter i = State (Set.Set (Term i)) ': Interpreter i

run :: proxy i
    -> Eff (Interpreter i) a
    -> Either String a
run _ f = State.runState f IntMap.empty
        & flip runReader Map.empty
        & runFailure
        & Effect.run
        & fmap fst

instance Bifunctor (Syntax i) where
  bimap f g s = case s of
    Var n -> Var (f n)
    Num v -> Num v
    Op1 o a -> Op1 o (g a)
    Op2 o a b -> Op2 o (g a) (g b)
    App a b -> App (g a) (g b)
    Lam n a -> Lam (f n) (g a)
    Rec n a -> Rec (f n) (g a)
    If0 c t e -> If0 (g c) (g t) (g e)

instance Functor (Syntax i n) where
  fmap = second

instance Eq i => Eq2 (Syntax i) where
  liftEq2 eqN eqA s1 s2 = case (s1, s2) of
    (Var n1, Var n2) -> eqN n1 n2
    (Num v1, Num v2) -> v1 == v2
    (Op1 o1 a1, Op1 o2 a2) -> o1 == o2 && eqA a1 a2
    (Op2 o1 a1 b1, Op2 o2 a2 b2) -> o1 == o2 && eqA a1 a2 && eqA b1 b2
    (App a1 b1, App a2 b2) -> eqA a1 a2 && eqA b1 b2
    (Lam n1 a1, Lam n2 a2) -> eqN n1 n2 && eqA a1 a2
    (Rec n1 a1, Rec n2 a2) -> eqN n1 n2 && eqA a1 a2
    (If0 c1 t1 e1, If0 c2 t2 e2) -> eqA c1 c2 && eqA t1 t2 && eqA e1 e2
    _ -> False

instance (Eq i, Eq n) => Eq1 (Syntax i n) where
  liftEq = liftEq2 (==)

instance Ord i => Ord2 (Syntax i) where
  liftCompare2 compareN compareA s1 s2
    | ordering <- compare (bimap (const ()) (const ()) s1) (bimap (const ()) (const ()) s2), ordering /= EQ = ordering
    | otherwise = case (s1, s2) of
      (Var n1, Var n2) -> compareN n1 n2
      (Num v1, Num v2) -> v1 `compare` v2
      (Op2 o1 a1 b1, Op2 o2 a2 b2) -> compare o1 o2 <> compareA a1 a2 <> compareA b1 b2
      (App a1 b1, App a2 b2) -> compareA a1 a2 <> compareA b1 b2
      (Lam n1 a1, Lam n2 a2) -> compareN n1 n2 <> compareA a1 a2
      (Rec n1 a1, Rec n2 a2) -> compareN n1 n2 <> compareA a1 a2
      (If0 c1 t1 e1, If0 c2 t2 e2) -> compareA c1 c2 <> compareA t1 t2 <> compareA e1 e2
      _ -> EQ

instance (Ord i, Ord n) => Ord1 (Syntax i n) where
  liftCompare = liftCompare2 compare


instance Show i => Show2 (Syntax i) where
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

instance (Show i, Show n) => Show1 (Syntax i n) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Num i => Num (Term i) where
  fromInteger = Fix . Num . fromInteger

  signum = Fix . Op1 Signum
  abs = Fix . Op1 Abs
  negate = Fix . Op1 Negate
  (+) = (Fix .) . Op2 Plus
  (*) = (Fix .) . Op2 Times

instance (Real i, AbstractValue i (Eff (Interpreter i))) => Real (Term i) where
  toRational term = case run (undefined :: proxy i) (fix ev term) of
    Right (I a) -> toRational a
    Right _ -> error "toRational applied to non-numeric Term"
    Left s -> error s
