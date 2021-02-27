module TeamTavern.Server.Domain.Paragraph where

import Prelude

import Data.Array (filter)
import Data.Either (fromRight)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), null, split, trim)
import Data.String as String
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequence)
import Data.Validated (Validated)
import Partial.Unsafe (unsafePartial)
import Wrapped.Validated as Wrapped

newtype Paragraph = Paragraph String

derive instance newtypeParagraph :: Newtype Paragraph _

create :: forall errors. String -> Validated (NonEmptyList errors) (Array Paragraph)
create text = let
    whitespaceRegex = regex """\s+""" global # unsafePartial fromRight
    paragraphs =
        text
        # split (Pattern "\n\n")
        <#> trim
        <#> replace whitespaceRegex " "
        # filter (not <<< null)
    in
    paragraphs <#> Wrapped.create identity [] Paragraph # sequence

length :: Paragraph -> Int
length = unwrap >>> String.length

toString :: Paragraph -> String
toString (Paragraph string) = string
