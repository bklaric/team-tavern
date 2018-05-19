module Wrapped.String where


import Prelude

import Data.Char.Unicode (isPrint, isSpace)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String (length, toCharArray)
import Data.Variant (SProxy(..), Variant, inj)

type TooLong = { original :: String, maxLength :: Int, actualLength :: Int }

tooLong :: forall errors.
    Int -> String -> Maybe (Variant (tooLong :: TooLong | errors))
tooLong maxLength string = let
    actualLength = length string
    in
    if actualLength <= maxLength
    then Nothing
    else Just
    $ inj (SProxy :: SProxy "tooLong")
    $ { original: string, maxLength, actualLength }

type NonPrintable = { original :: String }

notPrintable :: forall errors.
    String -> Maybe (Variant ( notPrintable :: NonPrintable | errors))
notPrintable string =
    if string # toCharArray # all isPrint
    then Nothing
    else Just $ inj (SProxy :: SProxy "notPrintable") { original: string }

type ContainsWhitespace = { original :: String }

containsWhitespace :: forall errors.
    String
    -> Maybe (Variant (containsWhitespace :: ContainsWhitespace | errors))
containsWhitespace string =
    if string # toCharArray # all (not <<< isSpace)
    then Nothing
    else Just $ inj (SProxy :: SProxy "containsWhitespace") { original: string }
