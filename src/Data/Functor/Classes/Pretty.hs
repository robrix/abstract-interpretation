module Data.Functor.Classes.Pretty
( Pretty1(..)
, Pretty2(..)
, pretty1
, prettyC
, pprint
) where

import Data.Foldable
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.IO (stdout)

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

instance Pretty1 Set.Set where
  liftPretty _ pl xs = pretty "Set.fromList" <+> pl (toList xs)

instance Pretty2 Either where
  liftPretty2 pL _ _ _ (Left l)  = pL l
  liftPretty2 _ _ pR _ (Right r) = pR r

instance Pretty l => Pretty1 (Either l) where
  liftPretty = liftPretty2 pretty prettyList

pprint :: Pretty a => a -> IO ()
pprint a = renderIO stdout (layoutPretty (LayoutOptions Unbounded) (pretty a <> pretty "\n"))

prettyC :: String -> [Doc ann] -> Doc ann
prettyC s fs = group (pretty s <> flatAlt (nest 2 (line <> vsep (map parens fs))) (space <> hsep (map parens fs)))
