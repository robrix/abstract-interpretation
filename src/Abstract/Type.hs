module Abstract.Type where

import Data.Text.Prettyprint.Doc

type TName = Int

data Type = Int | Bool | Type :-> Type | Type :* Type | TVar TName
  deriving (Eq, Ord, Show)


class Monad m => MonadFresh m where
  fresh :: m TName


instance Pretty Type where
  pretty = pretty . show
