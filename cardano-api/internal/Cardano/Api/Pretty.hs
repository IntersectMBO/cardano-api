module Cardano.Api.Pretty
  ( Ann
  , Doc
  , Pretty(..)
  , ShowOf(..)
  , viaShow
  , docToLazyText
  , docToText
  , docToString
  , pshow

  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  ) where

import           Cardano.Api.Via.ShowOf

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import           Prettyprinter
import           Prettyprinter.Render.Terminal

-- | 'Ann' is the prettyprinter annotation for cardano-api and cardano-cli to enable the printing
-- of colored output. This is a type alias for AnsiStyle.
type Ann = AnsiStyle

docToString :: Doc AnsiStyle -> String
docToString =  show

docToLazyText :: Doc AnsiStyle -> TextLazy.Text
docToLazyText = renderLazy . layoutPretty defaultLayoutOptions

docToText :: Doc AnsiStyle -> Text.Text
docToText = TextLazy.toStrict . docToLazyText

black :: Doc AnsiStyle -> Doc AnsiStyle
black = annotate (color Black)

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Blue)

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Magenta)

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Cyan)

white :: Doc AnsiStyle -> Doc AnsiStyle
white = annotate (color White)

pshow :: Show a => a -> Doc ann
pshow = viaShow
