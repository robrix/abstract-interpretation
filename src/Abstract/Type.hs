{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Abstract.Type where

import Control.Effect
import Control.Monad.Effect.Internal
import Data.Text.Prettyprint.Doc

type TName = Int

data Type = Int | Bool | Type :-> Type | Type :* Type | TVar TName
  deriving (Eq, Ord, Show)


data Fresh a where
  Reset :: Int -> Fresh ()
  Fresh :: Fresh Int

class Monad m => MonadFresh m where
  fresh :: m TName

instance Fresh :< fs => MonadFresh (Eff fs) where
  fresh = send Fresh


instance Pretty Type where
  pretty = pretty . show


instance RunEffect Fresh a where
  runEffect = relayState (0 :: TName) (const pure) (\ s action k -> case action of
    Fresh -> k (succ s) s
    Reset s' -> k s' ())
