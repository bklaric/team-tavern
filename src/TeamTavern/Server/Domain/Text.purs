module TeamTavern.Server.Domain.Text (Text, TextErrorRow, TextError, TextErrors, validateText) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Traversable (sum)
import Data.Validated (Validated, invalid, valid)
import Data.Variant (Variant, inj)
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.Paragraph as Paragraph
import Type.Proxy (Proxy(..))
import Wrapped.String (TooLong)

newtype Text = Text (Array Paragraph)

type TextErrorRow = (tooLong :: TooLong)

type TextError = Variant TextErrorRow

type TextErrors = NonEmptyArray TextError

create' :: Array Paragraph -> Validated TextErrors Text
create' paragraphs = let
    maxLength = 2000
    actualLength = paragraphs <#> Paragraph.length # sum
    in
    if actualLength > maxLength
    then invalid $ Nea.singleton $ inj (Proxy :: _ "tooLong")
        { maxLength, actualLength }
    else valid $ Text paragraphs

validateText :: String -> Validated TextErrors Text
validateText text = text # Paragraph.create >>= create'
