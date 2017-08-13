module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Store
import Abstract.Value
import qualified Data.Map as Map

type Cache l a = Map.Map (Configuration l a) (Value l a, Store l (Value l a))
