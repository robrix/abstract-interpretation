module Data.Functor.Classes.Pretty
( Pretty1(..)
, Pretty2(..)
, pretty1
, prettyC
, pprint
, Pretty1Of(..)
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

instance Pretty2 (,) where
  liftPretty2 pA _ pB _ (a, b) = tupled [ pA a, pB b ]

instance Pretty a => Pretty1 ((,) a) where
  liftPretty = liftPretty2 pretty prettyList

pprint :: Pretty a => a -> IO ()
pprint a = renderIO stdout (layoutPretty (LayoutOptions Unbounded) (pretty a <> pretty "\n"))

prettyC :: String -> [Doc ann] -> Doc ann
prettyC s fs = group (pretty s <> flatAlt (nest 2 (line <> vsep (map parens fs))) (space <> hsep (map parens fs)))


newtype Pretty1Of f a = Pretty1Of { unPretty1Of :: f a }
  deriving (Eq, Ord, Show)

instance Pretty1 f => Pretty1 (Pretty1Of f) where
  liftPretty p pl = liftPretty p pl . unPretty1Of

instance (Pretty1 f, Pretty a) => Pretty (Pretty1Of f a) where
  pretty = pretty1


newtype Pretty2Of f a b = Pretty2Of { unPretty2Of :: f a b }
  deriving (Eq, Ord, Show)

instance Pretty2 f => Pretty2 (Pretty2Of f) where
  liftPretty2 pA plA pB plB = liftPretty2 pA plA pB plB . unPretty2Of

instance (Pretty2 f, Pretty a) => Pretty1 (Pretty2Of f a) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty2 f, Pretty a, Pretty b) => Pretty (Pretty2Of f a b) where
  pretty = pretty1
