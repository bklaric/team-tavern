module Wrapped.String where

import Prelude

import Data.Char.Unicode (isDigit, isHexDigit, isPrint, toLower)
import Data.Foldable (all, any)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, null, split)
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
    { exactLength :: Int, actualLength :: Int }

notExactlyLong :: forall errors.
    Int -> String -> Maybe (Variant (notExactlyLong :: NotExactlyLong | errors))
notExactlyLong exactLength string = let
    actualLength = length string
    in
    if actualLength == exactLength
    then Nothing
    else Just
        $ inj (SProxy :: SProxy "notExactlyLong")
        { exactLength, actualLength }

type NotPrintable = {}

notPrintable :: forall errors.
    String -> Maybe (Variant ( notPrintable :: NotPrintable | errors))
notPrintable string =
    if string # toCharArray # all isPrint
    then Nothing
    else Just $ inj (SProxy :: SProxy "notPrintable") {}

type NotAsciiAlphaNum = {}

isAsciiAlpha :: Char -> Boolean
isAsciiAlpha ch = between 'a' 'z' (toLower ch)

notAsciiAlphaNum :: forall errors.
    String -> Maybe (Variant (notAsciiAlphaNum :: NotAsciiAlphaNum | errors))
notAsciiAlphaNum string =
    if string # toCharArray # all (\char -> isAsciiAlpha char || isDigit char)
    then Nothing
    else Just $ inj (SProxy :: SProxy "notAsciiAlphaNum") {}

type NotAsciiAlphaNumUnderscore = {}

notAsciiAlphaNumUnderscore :: forall errors.
  String
  -> Maybe (Variant
    ( notAsciiAlphaNumUnderscore :: NotAsciiAlphaNumUnderscore
    | errors
    ))
notAsciiAlphaNumUnderscore string =
    if string # toCharArray # all
        (\char -> isAsciiAlpha char || isDigit char || char == '_')
    then Nothing
    else Just $ inj (SProxy :: SProxy "notAsciiAlphaNumUnderscore") {}

type ContainsWhitespace = {}

isSpace :: String -> Boolean
isSpace char = any (char == _)  ["\t", "\n", "\r", "\f", "\v", " "]

containsWhitespace :: forall errors.
    String
    -> Maybe (Variant (containsWhitespace :: ContainsWhitespace | errors))
containsWhitespace string =
    if string # split (Pattern "") # all (not <<< isSpace)
    then Nothing
    else Just $ inj (SProxy :: SProxy "containsWhitespace") {}

type NotHex = {}

notHex :: forall errors.
    String -> Maybe (Variant (notHex :: NotHex | errors))
notHex string =
    if string # toCharArray # all isHexDigit
    then Nothing
    else Just $ inj (SProxy :: SProxy "notHex") {}
