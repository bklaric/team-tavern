module TeamTavern.Profile.Domain.Summary where

import Prelude

import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Traversable (sum)
import Data.Validated (Validated, invalid, valid)
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Domain.Paragraph (Paragraph)
import TeamTavern.Domain.Paragraph as Paragraph
import Wrapped.String (Empty, NotPrintable, TooLong)

newtype Summary = Summary (Array Paragraph)

derive instance newtypeSummary :: Newtype Summary _

type SummaryError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    )

maxLength :: Int
maxLength = 2000

createSummary :: Array Paragraph -> Validated (NonEmptyList SummaryError) Summary
createSummary paragraphs = let
    actualLength = paragraphs <#> Paragraph.length # sum
    in
    if actualLength > maxLength
    then invalid $ singleton $ inj (SProxy :: SProxy "tooLong")
        { maxLength, actualLength }
    else if actualLength == 0
    then invalid $ singleton $ inj (SProxy :: SProxy "empty") {}
    else valid $ Summary paragraphs

create :: String -> Validated (NonEmptyList SummaryError) Summary
create text = text # Paragraph.create >>= createSummary
