{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abstract.Syntax where

import Abstract.Value
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Semigroup
import qualified Data.Set as Set

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


-- Instances

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
