{-# LANGUAGE MultiParamTypeClasses #-}
module Abstract.GarbageCollection where

import Abstract.Set
import Abstract.Store

class MonadGC l a m where
  askRoots :: m (Set (Address l a))
