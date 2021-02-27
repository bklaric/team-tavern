module TeamTavern.Server.Domain.Text (Text, TextErrorRow, TextError, TextErrors, create, validateText) where

import Prelude

import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Traversable (sum)
import Data.Validated (Validated, invalid, valid)
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.Paragraph as Paragraph
import Wrapped.String (TooLong)

newtype Text = Text (Array Paragraph)

type TextErrorRow = (tooLong :: TooLong)

type TextError = Variant TextErrorRow

type TextErrors = NonEmptyList TextError

create'
    :: forall text
    .  Int
    -> (Array Paragraph -> text)
    -> Array Paragraph
    -> Validated (NonEmptyList TextError) text
create' maxLength constructor paragraphs = let
    actualLength = paragraphs <#> Paragraph.length # sum
    in
    if actualLength > maxLength
    then invalid $ singleton $ inj (SProxy :: SProxy "tooLong")
        { maxLength, actualLength }
    else valid $ constructor paragraphs

create :: forall text.
    Int -> (Array Paragraph -> text) -> String -> Validated TextErrors text
create maxLength constructor text =
    text # Paragraph.create >>= create' maxLength constructor

validateText :: String -> Validated TextErrors Text
validateText = create 2000 Text
