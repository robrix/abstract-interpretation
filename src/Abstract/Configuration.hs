module Abstract.Configuration where

import Abstract.Store
import Abstract.Syntax
import Abstract.Value

type Configuration l i = (Term i, Environment (l (Value l i)), Store l (Value l i))
