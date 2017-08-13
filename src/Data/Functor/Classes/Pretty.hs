module Data.Functor.Classes.Pretty where

import Data.Text.Prettyprint.Doc

class Pretty1 f where
  liftPretty :: (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> Doc ann
  liftPrettyList :: (a -> Doc ann) -> ([a] -> Doc ann) -> [f a] -> Doc ann
  liftPrettyList p pl = encloseSep lbracket rbracket (comma <> space) . map (liftPretty p pl)

class Pretty2 f where
  liftPretty2 :: (a -> Doc ann) -> ([a] -> Doc ann) -> (b -> Doc ann) -> ([b] -> Doc ann) -> f a b -> Doc ann
  liftPrettyList2 :: (a -> Doc ann) -> ([a] -> Doc ann) -> (b -> Doc ann) -> ([b] -> Doc ann) -> [f a b] -> Doc ann
  liftPrettyList2 pa pla pb plb = encloseSep lbracket rbracket (comma <> space) . map (liftPretty2 pa pla pb plb)

pretty1 :: (Pretty a, Pretty1 f) => f a -> Doc ann
pretty1 = liftPretty pretty prettyList

instance Pretty1 [] where
  liftPretty _ pl = pl
