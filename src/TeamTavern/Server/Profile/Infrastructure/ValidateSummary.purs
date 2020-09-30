module TeamTavern.Server.Profile.Infrastructure.ValidateSummary (Summary, validate, toStringArray) where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Validated (Validated)
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.Paragraph as Paragraph
import TeamTavern.Server.Domain.Text (TextError)
import TeamTavern.Server.Domain.Text as Text

newtype Summary = Summary (Array Paragraph)

maxLength :: Int
maxLength = 2000

validate :: String -> Validated (NonEmptyList TextError) Summary
validate = Text.create maxLength Summary

toStringArray :: Summary -> Array String
toStringArray (Summary paragraphs) = paragraphs <#> Paragraph.toString
