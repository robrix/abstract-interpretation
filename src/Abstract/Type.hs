module Abstract.Type where

import Data.Text.Prettyprint.Doc

data Type = Int | Bool
  deriving (Eq, Ord, Show)


instance Pretty Type where
  pretty = pretty . show
