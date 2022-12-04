module TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, toString, isSteamIdValid, validateSteamId) where

import Prelude

import Data.CodePoint.Unicode (isNumber)
import Data.Foldable (all)
import Data.Maybe (Maybe)
import Data.String (length, toCodePointArray)
import TeamTavern.Server.Infrastructure.Error (ValidatedTavern)
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)
import Type.Proxy (Proxy(..))

newtype SteamId = SteamId String

toString :: SteamId -> String
toString (SteamId steamId) = steamId

exactLength :: Int
exactLength = 17

isSteamIdValid :: String -> Boolean
isSteamIdValid steamId = length steamId == exactLength && (toCodePointArray steamId # all isNumber)

validateSteamId :: forall errors.
    Maybe String -> ValidatedTavern (steamId :: {} | errors) (Maybe SteamId)
validateSteamId steamId =
    validateContact steamId isSteamIdValid SteamId (Proxy :: _ "steamId") ("Invalid SteamId: " <> _)
