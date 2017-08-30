module Abstract.Type where

import Data.Text.Prettyprint.Doc

type TName = String

data Type = Int | Bool | Type :-> Type | Type :* Type | TVar TName
  deriving (Eq, Ord, Show)


instance Pretty Type where
  pretty = pretty . show
