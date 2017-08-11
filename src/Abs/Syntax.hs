module Abs.Syntax where

import Data.Functor.Foldable

data Syntax n a
  = Var n
  | Num Int
  | Op2 Op2 a a
  | App a a
  | Lam n a
  | Rec n a
  | If0 a a a

type Term = Fix (Syntax String)

data Op2 = Plus | Minus | Times | DividedBy
