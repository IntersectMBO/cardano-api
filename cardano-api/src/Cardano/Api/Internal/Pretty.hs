{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Internal.Pretty
  ( Ann
  , Doc
  , Pretty (..)
  , ShowOf (..)
  , docToLazyText
  , docToText
  , docToString
  , prettyShow
  , textShow
  , pshow
  , prettyException
  , hsep
  , vsep
  , (<+>)
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  )
where

import Cardano.Api.Internal.Via.ShowOf

import Cardano.Ledger.BaseTypes qualified as L

import Control.Exception.Safe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextLazy
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | 'Ann' is the prettyprinter annotation for cardano-api and cardano-cli to enable the printing
-- of colored output. This is a type alias for AnsiStyle.
type Ann = AnsiStyle

docToString :: Doc AnsiStyle -> String
docToString = show

-- | Render a 'Pretty' to a 'String'
prettyShow :: Pretty a => a -> String
prettyShow = docToString . pretty

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

-- | Short hand for 'viaShow'.
pshow :: Show a => a -> Doc ann
pshow = viaShow

-- | Short hand for @'pretty' . 'displayException'@
prettyException :: Exception a => a -> Doc ann
prettyException = pretty . displayException

textShow :: Show a => a -> Text
textShow = Text.pack . show

instance Pretty L.Url where
  pretty url = pretty (L.urlToText url)
