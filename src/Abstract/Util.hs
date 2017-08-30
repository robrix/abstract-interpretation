module Abstract.Util where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

prettyString :: Pretty a => a -> String
prettyString a = renderString (layoutPretty (LayoutOptions { layoutPageWidth = AvailablePerLine 100 1 }) (unAnnotate (pretty a)))
