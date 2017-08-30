{-# LANGUAGE FlexibleContexts, TypeOperators, UndecidableInstances #-}
module Abstract.Type where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh as Fresh
import Data.Text.Prettyprint.Doc

type TName = Int

data Type = Int | Bool | Type :-> Type | Type :* Type | TVar TName
  deriving (Eq, Ord, Show)


class Monad m => MonadFresh m where
  fresh :: m TName

instance Fresh :< fs => MonadFresh (Eff fs) where
  fresh = Fresh.fresh


instance Pretty Type where
  pretty = pretty . show
