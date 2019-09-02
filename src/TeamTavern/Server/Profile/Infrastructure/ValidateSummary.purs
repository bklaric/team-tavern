module TeamTavern.Server.Profile.Infrastructure.ValidateSummary (Summary, validate, toStringArray) where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Validated (Validated)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Domain.NonEmptyText as NonEmptyText
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.Paragraph as Paragraph

newtype Summary = Summary (Array Paragraph)

maxLength :: Int
maxLength = 2000

validate :: String -> Validated (NonEmptyList NonEmptyTextError) Summary
validate = NonEmptyText.create maxLength Summary

toStringArray :: Summary -> Array String
toStringArray (Summary paragraphs) = paragraphs <#> Paragraph.toString
