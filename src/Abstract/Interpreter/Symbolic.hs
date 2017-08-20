module Abstract.Interpreter.Symbolic where

import Abstract.Interpreter
data Sym a = Sym String | V a
  deriving (Eq, Ord, Show)

evSymbolic :: (Eval l fs (Sym a) -> Eval l fs (Sym a)) -> Eval l fs (Sym a) -> Eval l fs (Sym a)
evSymbolic ev0 ev e = ev0 ev e
