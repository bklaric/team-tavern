module Wrapped.String where

import Prelude

import Data.Char.Unicode (isHexDigit, isPrint, isSpace)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String (length, null)
import Data.String.CodeUnits (toCharArray)
import Data.Variant (SProxy(..), Variant, inj)

type Empty = {}

empty :: forall errors. String -> Maybe (Variant (empty :: Empty | errors))
empty string =
    if null string
    then Just $ inj (SProxy :: SProxy "empty") {}
    else Nothing

type TooLong = { maxLength :: Int, actualLength :: Int }

tooLong :: forall errors.
    Int -> String -> Maybe (Variant (tooLong :: TooLong | errors))
tooLong maxLength string = let
    actualLength = length string
    in
    if actualLength <= maxLength
    then Nothing
    else Just
        $ inj (SProxy :: SProxy "tooLong")
        { maxLength, actualLength }

type NotExactlyLong =
    { original :: String, exactLength :: Int, actualLength :: Int }

notExactlyLong :: forall errors.
    Int -> String -> Maybe (Variant (notExactlyLong :: NotExactlyLong | errors))
notExactlyLong exactLength string = let
    actualLength = length string
    in
    if actualLength == exactLength
    then Nothing
    else Just
        $ inj (SProxy :: SProxy "notExactlyLong")
        { original: string, exactLength, actualLength }

type NotPrintable = {}

notPrintable :: forall errors.
    String -> Maybe (Variant ( notPrintable :: NotPrintable | errors))
notPrintable string =
    if string # toCharArray # all isPrint
    then Nothing
    else Just $ inj (SProxy :: SProxy "notPrintable") {}

type ContainsWhitespace = {}

containsWhitespace :: forall errors.
    String
    -> Maybe (Variant (containsWhitespace :: ContainsWhitespace | errors))
containsWhitespace string =
    if string # toCharArray # all (not <<< isSpace)
    then Nothing
    else Just $ inj (SProxy :: SProxy "containsWhitespace") {}

type NotHex = { original :: String }

notHex :: forall errors.
    String -> Maybe (Variant (notHex :: NotHex | errors))
notHex string =
    if string # toCharArray # all isHexDigit
    then Nothing
    else Just $ inj (SProxy :: SProxy "notHex") { original: string }
