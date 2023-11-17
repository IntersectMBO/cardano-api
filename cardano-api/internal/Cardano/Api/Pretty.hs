module Cardano.Api.Pretty
  ( Ann
  , Doc
  , Pretty(..)
  , ShowOf(..)
  , viaShow
  , prettyToLazyText
  , prettyToText
  , prettyToString
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

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import           Prettyprinter
import           Prettyprinter.Render.Terminal

type Ann = AnsiStyle

newtype ShowOf a = ShowOf a

instance Show a => Show (ShowOf a) where
  show (ShowOf a) = show a

instance Show a => Pretty (ShowOf a) where
  pretty = viaShow

prettyToString :: Doc AnsiStyle -> String
prettyToString =  show

prettyToLazyText :: Doc AnsiStyle -> TextLazy.Text
prettyToLazyText = renderLazy . layoutPretty defaultLayoutOptions

prettyToText :: Doc AnsiStyle -> Text.Text
prettyToText = TextLazy.toStrict . prettyToLazyText

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
pshow = pretty . show
