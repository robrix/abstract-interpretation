{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Symbolic where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Syntax
import Abstract.Value
import Control.Applicative
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.Amb
import Control.Monad.Effect.State
import qualified Data.Set as Set

data Sym t a = Sym t | V a
  deriving (Eq, Ord, Show)

sym :: (Num a, Num t) => (forall n . Num n => n -> n) -> Sym t a -> Sym t a
sym f (Sym t) = Sym (f t)
sym f (V a) = V (f a)

sym2 :: Applicative f => (a -> a -> f a) -> (a -> t) -> (t -> t -> t) -> Sym t a -> Sym t a -> f (Sym t a)
sym2 f _ _ (V a) (V b) = V <$> f a b
sym2 _ _ g (Sym a) (Sym b) = pure (Sym (g a b))
sym2 f num g a (V b) = sym2 f num g a (Sym (num b))
sym2 f num g (V a) b = sym2 f num g (Sym (num a)) b

evSymbolic :: (Eval t fs (Value l t (Sym t a)) -> Eval t fs (Value l t (Sym t a))) -> Eval t fs (Value l t (Sym t a)) -> Eval t fs (Value l t (Sym t a))
evSymbolic ev0 ev e = ev0 ev e


data PathExpression a = E (Term a) | NotE (Term a)
  deriving (Eq, Ord, Show)

newtype PathCondition a = PathCondition { unPathCondition :: Set.Set (PathExpression a) }
  deriving (Eq, Ord, Show)

getPathCondition :: State (PathCondition a) :< fs => Eff fs (PathCondition a)
getPathCondition = get

refine :: (Ord a, State (PathCondition a) :< fs) => PathExpression a -> Eff fs ()
refine = modify . pathConditionInsert

pathConditionMember :: Ord a => PathExpression a -> PathCondition a -> Bool
pathConditionMember = (. unPathCondition) . Set.member

pathConditionInsert :: Ord a => PathExpression a -> PathCondition a -> PathCondition a
pathConditionInsert = ((PathCondition .) . (. unPathCondition)) . Set.insert


instance (Num a, Ord a, Primitive a (Eff fs), State (PathCondition a) :< fs, Amb :< fs) => Primitive (Sym (Term a) a) (Eff fs) where
  delta1 o a = case o of
    Negate -> pure (sym negate a)
    Abs    -> pure (sym abs    a)
    Signum -> pure (sym signum a)
    Not    -> case a of
      Sym t -> pure (Sym (In (Op1 Not t)))
      V a -> V <$> delta1 Not a

  delta2 o a b = case o of
    Plus  -> sym2 (delta2 Plus ) prim (+) a b
    Minus -> sym2 (delta2 Minus) prim (-) a b
    Times -> sym2 (delta2 Times) prim (*) a b
    DividedBy -> isZero b >>= flip when divisionByZero >> sym2 (delta2 DividedBy) prim ((In .) . Op2 DividedBy) a b
    Quotient  -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Quotient)  prim ((In .) . Op2 Quotient)  a b
    Remainder -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Remainder) prim ((In .) . Op2 Remainder) a b
    Modulus   -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Modulus)   prim ((In .) . Op2 Modulus)   a b
    And -> sym2 (delta2 And) prim ((In .) . Op2 And) a b
    Or  -> sym2 (delta2 Or)  prim ((In .) . Op2 Or)  a b
    XOr -> sym2 (delta2 XOr) prim ((In .) . Op2 XOr) a b
    Eq  -> sym2 (delta2 Eq)  prim ((In .) . Op2 Eq) a b

  isZero (V a) = isZero a
  isZero (Sym e) = do
    phi <- getPathCondition
    if E e `pathConditionMember` phi then
      return True
    else if NotE e `pathConditionMember` phi then
      return False
    else
        ((refine (E e)    >> return True)
     <|> (refine (NotE e) >> return False))

  truthy (V a) = truthy a
  truthy (Sym e) = do
    phi <- getPathCondition
    if E e `pathConditionMember` phi then
      return True
    else if NotE e `pathConditionMember` phi then
      return False
    else
        ((refine (E e)    >> return True)
     <|> (refine (NotE e) >> return False))
