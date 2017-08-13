module Data.Functor.Classes.Pretty where

import Data.Text.Prettyprint.Doc

class Pretty1 f where
  liftPretty :: (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> Doc ann
  liftPrettyList :: (a -> Doc ann) -> ([a] -> Doc ann) -> [f a] -> Doc ann
