module Wrapped.String where

import Prelude

import Data.CodePoint.Unicode (isDecDigit, isHexDigit, isPrint, toLowerSimple)
import Data.Foldable (all, any)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, Pattern(..), codePointFromChar, length, null, split, toCodePointArray)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type Empty = {}

empty :: ∀ errors. String -> Maybe (Variant (empty :: Empty | errors))
empty string =
    if null string
    then Just $ inj (Proxy :: _ "empty") {}
    else Nothing

type TooShort = { minLength :: Int, actualLength :: Int }

tooShort :: ∀ errors.
    Int -> String -> Maybe (Variant (tooShort :: TooShort | errors))
tooShort minLength string = let
    actualLength = length string
    in
    if actualLength >= minLength
    then Nothing
    else Just $ inj (Proxy :: _ "tooShort") { minLength, actualLength }

type TooLong = { maxLength :: Int, actualLength :: Int }

tooLong :: ∀ errors.
    Int -> String -> Maybe (Variant (tooLong :: TooLong | errors))
tooLong maxLength string = let
    actualLength = length string
    in
    if actualLength <= maxLength
    then Nothing
    else Just $ inj (Proxy :: _ "tooLong") { maxLength, actualLength }

type NotExactlyLong =
    { exactLength :: Int, actualLength :: Int }

notExactlyLong :: ∀ errors.
    Int -> String -> Maybe (Variant (notExactlyLong :: NotExactlyLong | errors))
notExactlyLong exactLength string = let
    actualLength = length string
    in
    if actualLength == exactLength
    then Nothing
    else Just $ inj (Proxy :: _ "notExactlyLong") { exactLength, actualLength }

type NotPrintable = {}

notPrintable :: ∀ errors.
    String -> Maybe (Variant (notPrintable :: NotPrintable | errors))
notPrintable string =
    if string # toCodePointArray # all isPrint
    then Nothing
    else Just $ inj (Proxy :: _ "notPrintable") {}

type NotAsciiAlphaNum = {}

isAsciiAlpha :: CodePoint -> Boolean
isAsciiAlpha ch = between (codePointFromChar 'a') (codePointFromChar 'z') (toLowerSimple ch)

notAsciiAlphaNum :: ∀ errors.
    String -> Maybe (Variant (notAsciiAlphaNum :: NotAsciiAlphaNum | errors))
notAsciiAlphaNum string =
    if string # toCodePointArray # all (\char -> isAsciiAlpha char || isDecDigit char)
    then Nothing
    else Just $ inj (Proxy :: _ "notAsciiAlphaNum") {}

type NotAsciiAlphaNumHyphen = {}

notAsciiAlphaNumHyphen :: ∀ errors.
    String -> Maybe (Variant (notAsciiAlphaNumHyphen :: NotAsciiAlphaNumHyphen | errors))
notAsciiAlphaNumHyphen string =
    if string # toCodePointArray # all
        (\char -> isAsciiAlpha char || isDecDigit char || char == (codePointFromChar '-'))
    then Nothing
    else Just $ inj (Proxy :: _ "notAsciiAlphaNumHyphen") {}

type NotAsciiAlphaNumSpecial = {}

notAsciiAlphaNumSpecial :: ∀ errors.
    String -> Maybe (Variant (notAsciiAlphaNumSpecial :: NotAsciiAlphaNumSpecial | errors))
notAsciiAlphaNumSpecial string =
    if string # toCodePointArray # all
        (\char -> isAsciiAlpha char || isDecDigit char || char == (codePointFromChar '-') || char == (codePointFromChar '_') || char == (codePointFromChar '.'))
    then Nothing
    else Just $ inj (Proxy :: _ "notAsciiAlphaNumSpecial") {}

type ContainsWhitespace = {}

isSpace :: String -> Boolean
isSpace char = any (char == _)  ["\t", "\n", "\r", "\x0c", "\x0b", " "]

containsWhitespace :: ∀ errors.
    String -> Maybe (Variant (containsWhitespace :: ContainsWhitespace | errors))
containsWhitespace string =
    if string # split (Pattern "") # all (not <<< isSpace)
    then Nothing
    else Just $ inj (Proxy :: _ "containsWhitespace") {}

type NotHex = {}

notHex :: ∀ errors. String -> Maybe (Variant (notHex :: NotHex | errors))
notHex string =
    if string # toCodePointArray # all isHexDigit
    then Nothing
    else Just $ inj (Proxy :: _ "notHex") {}

type Invalid = { original :: String }

invalid :: ∀ errors.
    (String -> Boolean) -> String -> Maybe (Variant (invalid :: Invalid | errors))
invalid check string =
    if check string
    then Nothing
    else Just $ inj (Proxy :: _ "invalid") { original: string }
