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
import Data.Variant (Variant)
import Partial.Unsafe (unsafePartial)
import Wrapped.String (NotPrintable, notPrintable)
import Wrapped.Validated as Wrapped

newtype Paragraph = Paragraph String

derive instance newtypeParagraph :: Newtype Paragraph _

type ParagraphError errors = Variant (notPrintable :: NotPrintable | errors)

create :: forall errors.
    String -> Validated (NonEmptyList (ParagraphError errors)) (Array Paragraph)
create text = let
    whitespaceRegex = regex """\s+""" global # unsafePartial fromRight
    paragraphs =
        text
        # split (Pattern "\n\n")
        <#> trim
        <#> replace whitespaceRegex " "
        # filter (not <<< null)
    in
    paragraphs <#> Wrapped.create identity [notPrintable] Paragraph # sequence

length :: Paragraph -> Int
length = unwrap >>> String.length
