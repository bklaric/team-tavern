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

type TooShort = { minLength :: Int, actualLength :: Int }

tooShort :: forall errors.
    Int -> String -> Maybe (Variant (tooShort :: TooShort | errors))
tooShort minLength string = let
    actualLength = length string
    in
    if actualLength >= minLength
    then Nothing
    else Just
        $ inj (SProxy :: SProxy "tooShort")
        { minLength, actualLength }

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

type NotAsciiAlphaNumHyphen = {}

notAsciiAlphaNumHyphen :: forall errors.
  String
  -> Maybe (Variant
    ( notAsciiAlphaNumHyphen :: NotAsciiAlphaNumHyphen
    | errors
    ))
notAsciiAlphaNumHyphen string =
    if string # toCharArray # all
        (\char -> isAsciiAlpha char || isDigit char || char == '-')
    then Nothing
    else Just $ inj (SProxy :: SProxy "notAsciiAlphaNumHyphen") {}

type NotAsciiAlphaNumSpecial = {}

notAsciiAlphaNumSpecial :: forall errors.
  String
  -> Maybe (Variant
    ( notAsciiAlphaNumSpecial :: NotAsciiAlphaNumSpecial
    | errors
    ))
notAsciiAlphaNumSpecial string =
    if string # toCharArray # all
        (\char -> isAsciiAlpha char || isDigit char || char == '-' || char == '_' || char == '.')
    then Nothing
    else Just $ inj (SProxy :: SProxy "notAsciiAlphaNumSpecial") {}

type ContainsWhitespace = {}

isSpace :: String -> Boolean
isSpace char = any (char == _)  ["\t", "\n", "\r", "\x0c", "\x0b", " "]

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


type Invalid = { original :: String }

invalid
    :: forall errors
    .  (String -> Boolean)
    -> String
    -> Maybe (Variant (invalid :: Invalid | errors))
invalid check string =
    if check string
    then Nothing
    else Just $ inj (SProxy :: SProxy "invalid") { original: string }
