module TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError, create) where

import Prelude

import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Traversable (sum)
import Data.Validated (Validated, invalid, valid)
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.Paragraph as Paragraph
import Wrapped.String (Empty, TooLong)

type NonEmptyTextError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    )

create'
    :: forall text
    .  Int
    -> (Array Paragraph -> text)
    -> Array Paragraph
    -> Validated (NonEmptyList NonEmptyTextError) text
create' maxLength constructor paragraphs = let
    actualLength = paragraphs <#> Paragraph.length # sum
    in
    if actualLength > maxLength
    then invalid $ singleton $ inj (SProxy :: SProxy "tooLong")
        { maxLength, actualLength }
    else if actualLength == 0
    then invalid $ singleton $ inj (SProxy :: SProxy "empty") {}
    else valid $ constructor paragraphs

create
    :: forall text
    .  Int
    -> (Array Paragraph -> text)
    -> String
    -> Validated (NonEmptyList NonEmptyTextError) text
create maxLength constructor text =
    text # Paragraph.create >>= create' maxLength constructor
