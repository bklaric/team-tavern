module TeamTavern.Server.Domain.Text (Text, TextErrorRow, TextError, TextErrors, create, validateText) where

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

create'
    :: forall text
    .  Int
    -> (Array Paragraph -> text)
    -> Array Paragraph
    -> Validated TextErrors text
create' maxLength constructor paragraphs = let
    actualLength = paragraphs <#> Paragraph.length # sum
    in
    if actualLength > maxLength
    then invalid $ Nea.singleton $ inj (Proxy :: _ "tooLong")
        { maxLength, actualLength }
    else valid $ constructor paragraphs

create :: forall text.
    Int -> (Array Paragraph -> text) -> String -> Validated TextErrors text
create maxLength constructor text =
    text # Paragraph.create >>= create' maxLength constructor

validateText :: String -> Validated TextErrors Text
validateText = create 2000 Text
