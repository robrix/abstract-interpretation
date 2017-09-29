{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Collecting where

import Abstract.Environment
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Abstract.Term
import Abstract.Value
import Abstract.Eval

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semigroup

class Monad m => MonadGC l a m where
  askRoots :: m (Set (Address l a))

  extraRoots :: Set (Address l a) -> m b -> m b

instance (Ord l, Reader (Set (Address l a)) :< fs) => MonadGC l a (Eff fs) where
  askRoots = ask :: Eff fs (Set (Address l a))

  extraRoots roots' = local (<> roots')


gc :: (Ord l, Foldable (Cell l), AbstractValue l a) => Set (Address l a) -> Store l a -> Store l a
gc roots store = storeRestrict store (reachable roots store)

reachable :: (Ord l, Foldable (Cell l), AbstractValue l a) => Set (Address l a) -> Store l a -> Set (Address l a)
reachable roots store = go roots mempty
  where go set seen = case split set of
          Nothing -> seen
          Just (a, as)
            | Just values <- storeLookupAll a store -> go (difference (foldr ((<>) . valueRoots) mempty values <> as) seen) (insert a seen)
            | otherwise -> go seen (insert a seen)


evCollect :: forall l t v m
          .  ( Ord l
             , Foldable (Cell l)
             , MonadStore l v m
             , MonadGC l v m
             , AbstractValue l v
             )
          => (Eval' t (m v) -> Eval' t (m v))
          -> Eval' t (m v)
          -> Eval' t (m v)
evCollect ev0 ev e = do
  roots <- askRoots :: m (Set (Address l v))
  v <- ev0 ev e
  modifyStore (gc (roots <> valueRoots v))
  return v

evRoots :: forall l v m s
        .  ( Ord l
           , MonadEnv l v m
           , MonadGC l v m
           , MonadPrim v m
           , AbstractValue l v
           , EvalCollect v m s
           )
        => (Eval' (Term s) (m v) -> Eval' (Term s) (m v))
        -> Eval' (Term s) (m v)
        -> Eval' (Term s) (m v)
evRoots = evalCollect
-- evRoots ev0 ev e = case out e of
--   If e0 e1 e2 -> do
--     env <- askEnv @l @v
--     v <- extraRoots (envRoots env (freeVariables e1 <> freeVariables e2)) (ev e0)
--     b <- truthy v
--     ev (if b then e1 else e2)
--   Op2 o e0 e1 -> do
--     env <- askEnv @l @v
--     v0 <- extraRoots (envRoots env (freeVariables e1)) (ev e0)
--     v1 <- extraRoots (valueRoots @l v0) (ev e1)
--     delta2 o v0 v1
--   App e0 e1 -> do
--     env <- askEnv @l @v
--     v0 <- extraRoots (envRoots env (freeVariables e1)) (ev e0)
--     v1 <- extraRoots (valueRoots @l v0) (ev e1)
--     app @l ev v0 v1
--   _ -> ev0 ev e


-- TODO: Will need some sort of new type class for ^.
-- class EvalCollect
