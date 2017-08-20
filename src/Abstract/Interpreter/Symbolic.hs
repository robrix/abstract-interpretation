{-# LANGUAGE RankNTypes #-}
module Abstract.Interpreter.Symbolic where

import Abstract.Interpreter
data Sym a = Sym String | V a
  deriving (Eq, Ord, Show)

sym :: Num a => (forall n . Num n => n -> n) -> Sym a -> Sym a
sym f (Sym t) = Sym (f t)
sym f (V a) = V (f a)

evSymbolic :: (Eval l fs (Sym a) -> Eval l fs (Sym a)) -> Eval l fs (Sym a) -> Eval l fs (Sym a)
evSymbolic ev0 ev e = ev0 ev e
