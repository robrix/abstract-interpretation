{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Abstract.Configuration where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value

data Configuration l i = Configuration { configurationTerm :: Term i, configurationEnvironment :: Environment (l (Value l i)), configurationStore :: Store l (Value l i) }

deriving instance (Eq i, Eq (l (Value l i)), Eq (Store l (Value l i))) => Eq (Configuration l i)
deriving instance (Ord i, Ord (l (Value l i)), Ord (Store l (Value l i))) => Ord (Configuration l i)
deriving instance (Show i, Show (l (Value l i)), Show (Store l (Value l i))) => Show (Configuration l i)
