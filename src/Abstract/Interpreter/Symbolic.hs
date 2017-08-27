{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Symbolic where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Syntax
import Control.Applicative
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
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

evSymbolic :: (Eval t (Eff fs (v (Sym t a))) -> Eval t (Eff fs (v (Sym t a)))) -> Eval t (Eff fs (v (Sym t a))) -> Eval t (Eff fs (v (Sym t a)))
evSymbolic ev0 ev e = ev0 ev e


data PathExpression t = E t | NotE t
  deriving (Eq, Ord, Show)

newtype PathCondition t = PathCondition { unPathCondition :: Set.Set (PathExpression t) }
  deriving (Eq, Ord, Show)


refine :: (Ord t, MonadPathCondition t m) => PathExpression t -> m ()
refine = modifyPathCondition . pathConditionInsert

pathConditionMember :: Ord t => PathExpression t -> PathCondition t -> Bool
pathConditionMember = (. unPathCondition) . Set.member

pathConditionInsert :: Ord t => PathExpression t -> PathCondition t -> PathCondition t
pathConditionInsert = ((PathCondition .) . (. unPathCondition)) . Set.insert


class Monad m => MonadPathCondition t m where
  getPathCondition :: m (PathCondition t)
  putPathCondition :: PathCondition t -> m ()

instance State (PathCondition t) :< fs => MonadPathCondition t (Eff fs) where
  getPathCondition = get
  putPathCondition = put

modifyPathCondition :: MonadPathCondition t m => (PathCondition t -> PathCondition t) -> m ()
modifyPathCondition f = getPathCondition >>= putPathCondition . f

instance (Num a, Ord a, Primitive a, MonadFail m, MonadPrim a m, MonadPathCondition (Term a) m, Alternative m) => MonadPrim (Sym (Term a) a) m where
  delta1 o a = case o of
    Negate -> pure (negate a)
    Abs    -> pure (abs a)
    Signum -> pure (signum a)
    Not    -> case a of
      Sym t -> pure (Sym (unary Not t))
      V a -> V <$> delta1 Not a

  delta2 o a b = case o of
    Plus  -> pure (a + b)
    Minus -> pure (a - b)
    Times -> pure (a * b)
    DividedBy -> isZero b >>= flip when divisionByZero >> sym2 (delta2 DividedBy) prim (binary DividedBy) a b
    Quotient  -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Quotient)  prim (binary Quotient)  a b
    Remainder -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Remainder) prim (binary Remainder) a b
    Modulus   -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Modulus)   prim (binary Modulus)   a b
    And -> sym2 (delta2 And) prim (binary And) a b
    Or  -> sym2 (delta2 Or)  prim (binary Or)  a b
    Eq  -> sym2 (delta2 Eq)  prim (binary Eq)  a b
    Lt  -> sym2 (delta2 Lt)  prim (binary Lt)  a b
    LtE -> sym2 (delta2 LtE) prim (binary LtE) a b
    Gt  -> sym2 (delta2 Gt)  prim (binary Gt)  a b
    GtE -> sym2 (delta2 GtE) prim (binary GtE) a b

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

instance Num a => Num (Sym (Term a) a) where
  fromInteger = V . fromInteger

  signum (V a)   = V   (signum a)
  signum (Sym t) = Sym (signum t)
  abs (V a)      = V   (abs    a)
  abs (Sym t)    = Sym (abs    t)
  negate (V a)   = V   (negate a)
  negate (Sym t) = Sym (negate t)
  V   a + V   b = V   (     a +      b)
  Sym a + V   b = Sym (     a + prim b)
  V   a + Sym b = Sym (prim a +      b)
  Sym a + Sym b = Sym (     a +      b)
  V   a - V   b = V   (     a -      b)
  Sym a - V   b = Sym (     a - prim b)
  V   a - Sym b = Sym (prim a -      b)
  Sym a - Sym b = Sym (     a -      b)
  V   a * V   b = V   (     a *      b)
  Sym a * V   b = Sym (     a * prim b)
  V   a * Sym b = Sym (prim a *      b)
  Sym a * Sym b = Sym (     a *      b)
