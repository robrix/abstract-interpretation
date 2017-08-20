{-# LANGUAGE RankNTypes #-}
module Abstract.Interpreter.Symbolic where

import Abstract.Interpreter
data Sym a = Sym String | V a
  deriving (Eq, Ord, Show)

sym :: Num a => (forall n . Num n => n -> n) -> Sym a -> Sym a
sym f (Sym t) = Sym (f t)
sym f (V a) = V (f a)

sym2 :: Num a => (forall n . Num n => n -> n -> n) -> Sym a -> Sym a -> Sym a
sym2 f (V a) (V b) = V (f a b)
sym2 f (Sym a) (Sym b) = Sym (f a b)
sym2 f (Sym a) (V b) = Sym (f a (num b))
sym2 f (V a) (Sym b) = Sym (f (num a) b)

evSymbolic :: (Eval l fs (Sym a) -> Eval l fs (Sym a)) -> Eval l fs (Sym a) -> Eval l fs (Sym a)
evSymbolic ev0 ev e = ev0 ev e
