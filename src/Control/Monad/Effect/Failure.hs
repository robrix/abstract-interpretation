{-# LANGUAGE GADTs #-}
module Control.Monad.Effect.Failure where

data Failure a where
  Failure :: String -> Failure a
