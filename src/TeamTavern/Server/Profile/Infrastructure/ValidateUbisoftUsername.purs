module TeamTavern.Server.Profile.Infrastructure.ValidateUbisoftUsername (UbisoftUsername, toString, validateUbisoftUsername) where

import Prelude

import Data.CodePoint.Unicode (isAlpha)
import Data.Maybe (Maybe, maybe)
import Data.String (codePointAt, length)
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNeaVar)
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)
import Type.Proxy (Proxy(..))

newtype UbisoftUsername = UbisoftUsername String

toString :: UbisoftUsername -> String
toString (UbisoftUsername ubisoftUsername) = ubisoftUsername

minUsernameLength :: Int
minUsernameLength = 3

maxUsernameLength :: Int
maxUsernameLength = 16

isUbisoftUsernameValid :: String -> Boolean
isUbisoftUsernameValid ubisoftUsername =
    minUsernameLength <= length ubisoftUsername
    && length ubisoftUsername <= maxUsernameLength
    && (codePointAt 0 ubisoftUsername <#> isAlpha # maybe false identity)

validateUbisoftUsername :: ∀ errors.
    Maybe String -> ValidatedTerrorNeaVar (ubisoftUsername :: {} | errors) (Maybe UbisoftUsername)
validateUbisoftUsername ubisoftUsername =
    validateContact ubisoftUsername isUbisoftUsernameValid UbisoftUsername (Proxy :: _ "ubisoftUsername") ("Invalid Ubisoft Connect username: " <> _)
