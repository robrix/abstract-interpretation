module Abs.Syntax where

data Syntax n a
  = Var n
  | Num Int
  | App a a
  | Lam n a
  | Rec n a
  | If0 a a a
