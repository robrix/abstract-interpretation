{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Abstract.Interpreter.Symbolic where

import Abstract.Interpreter
import Abstract.Number
import Abstract.Syntax
import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import qualified Data.Set as Set

data Sym a = Sym (Term a) | V a
  deriving (Eq, Ord, Show)

sym :: Num a => (forall n . Num n => n -> n) -> Sym a -> Sym a
sym f (Sym t) = Sym (f t)
sym f (V a) = V (f a)

sym2 :: Applicative f => (a -> a -> f a) -> (Term a -> Term a -> Term a) -> Sym a -> Sym a -> f (Sym a)
sym2 f _ (V a) (V b) = V <$> f a b
sym2 _ g (Sym a) (Sym b) = pure (Sym (g a b))
sym2 f g a (V b) = sym2 f g a (Sym (num b))
sym2 f g (V a) b = sym2 f g (Sym (num a)) b

evSymbolic :: (Eval l fs (Sym a) -> Eval l fs (Sym a)) -> Eval l fs (Sym a) -> Eval l fs (Sym a)
evSymbolic ev0 ev e = ev0 ev e


data PathExpression a = E (Term a) | NotE (Term a)
  deriving (Eq, Ord, Show)

newtype PathCondition a = PathCondition { unPathCondition :: Set.Set (PathExpression a) }
  deriving (Eq, Ord, Show)

getPathCondition :: MonadState (PathCondition a) m => m (PathCondition a)
getPathCondition = get

refine :: (Ord a, MonadState (PathCondition a) m) => PathExpression a -> m ()
refine = modify . pathConditionInsert

pathConditionMember :: Ord a => PathExpression a -> PathCondition a -> Bool
pathConditionMember = (. unPathCondition) . Set.member

pathConditionInsert :: Ord a => PathExpression a -> PathCondition a -> PathCondition a
pathConditionInsert = ((PathCondition .) . (. unPathCondition)) . Set.insert


instance (Num a, Ord a, AbstractNumber a m, MonadState (PathCondition a) m, Alternative m) => AbstractNumber (Sym a) m where
  delta1 o a = pure $ case o of
    Negate -> sym negate a
    Abs    -> sym abs    a
    Signum -> sym signum a

  delta2 o a b = case o of
    Plus  -> sym2 (delta2 Plus ) (+) a b
    Minus -> sym2 (delta2 Minus) (-) a b
    Times -> sym2 (delta2 Times) (*) a b
    DividedBy -> isZero b >>= flip when divisionByZero >> sym2 (delta2 DividedBy) ((In .) . Op2 DividedBy) a b
    Quotient  -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Quotient)  ((In .) . Op2 Quotient)  a b
    Remainder -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Remainder) ((In .) . Op2 Remainder) a b

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
