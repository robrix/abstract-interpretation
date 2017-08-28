{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Collecting where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Abstract.Syntax
import Abstract.Value
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semigroup

class Monad m => MonadGC l a m where
  askRoots :: m (Set (Address l a))

  extraRoots :: Set (Address l a) -> m b -> m b

instance (Ord l, Reader (Roots l a) :< fs) => MonadGC l a (Eff fs) where
  askRoots = fmap rootsSet ask

  extraRoots roots = local (modifyRootsSet (<> roots))


data Roots l a = Roots { rootsSet :: Set (Address l a), rootsRho :: () }

modifyRootsSet :: (Set (Address l a) -> Set (Address l a)) -> Roots l a -> Roots l a
modifyRootsSet f r = r { rootsSet = f (rootsSet r) }


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
          .  (Ord l, Foldable (Cell l), MonadStore l v m, MonadGC l v m, MonadValue l v t m)
          => (Eval t (m v) -> Eval t (m v))
          -> Eval t (m v)
          -> Eval t (m v)
evCollect ev0 ev e = do
  roots <- askRoots :: m (Set (Address l v))
  v <- ev0 ev e
  modifyStore (gc (roots <> valueRoots v))
  return v


instance Ord l => Semigroup (Roots l a) where
  r1 <> r2 = Roots (rootsSet r1 <> rootsSet r2) (rootsRho r1 <> rootsRho r2)

instance Ord l => Monoid (Roots l a) where
  mempty = Roots mempty mempty
  mappend = (<>)
