module TeamTavern.Server.Domain.Paragraph where

import Prelude

import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (fromRight)
import Data.String (Pattern(..), null, split, trim)
import Data.String as String
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequence)
import Data.Validated (Validated)
import Undefined (undefined)
import Wrapped.Validated as Wrapped

newtype Paragraph = Paragraph String

create :: forall errors.
    String -> Validated (NonEmptyArray errors) (Array Paragraph)
create text = let
    whitespaceRegex = regex """\s+""" global # fromRight undefined
    paragraphs =
        text
        # split (Pattern "\n\n")
        <#> trim
        <#> replace whitespaceRegex " "
        # filter (not <<< null)
    in
    paragraphs <#> Wrapped.create identity [] Paragraph # sequence

length :: Paragraph -> Int
length (Paragraph string) = String.length string

toString :: Paragraph -> String
toString (Paragraph string) = string
