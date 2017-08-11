module Abs.Syntax where

data Syntax n a
  = Var n
  | Num Int
  | App a a
  | Lam x a
  | Rec x a
  | If0 a a a
