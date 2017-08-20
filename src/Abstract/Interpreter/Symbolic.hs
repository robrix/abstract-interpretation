module Abstract.Interpreter.Symbolic where

data Sym a = Sym String | V a
  deriving (Eq, Ord, Show)
