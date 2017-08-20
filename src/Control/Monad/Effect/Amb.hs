{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Effect.Amb where

import Control.Applicative
import Control.Monad.Effect.Internal
import Data.Foldable (asum)

-- | Ambiguity (aka nondeterminism) effect.
--
--   This effect avoids nontermination observed with NonDetEff.
data Amb a where
  Amb :: [a] -> Amb a

instance Amb :< fs => Alternative (Eff fs) where
  empty = send (Amb [])
  a <|> b = interpose pure (\ (Amb as) ka -> interpose pure (\ (Amb bs) kb -> (++) <$> traverse ka as <*> traverse kb bs >>= send . Amb) b) a

runAmb :: Alternative f => Eff (Amb ': fs) a -> Eff fs (f a)
runAmb = relay (pure . pure) (\ (Amb as) k -> fmap asum (traverse k as))
