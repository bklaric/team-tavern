module TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, toString, isSteamIdValid, validateSteamId) where

import Prelude

import Data.Char.Unicode (isNumber)
import Data.Foldable (all)
import Data.Maybe (Maybe)
import Data.String (length)
import Data.String.CodeUnits (toCharArray)
import Data.Validated.Label (ValidatedVariants)
import Data.Variant (SProxy(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)

newtype SteamId = SteamId String

toString :: SteamId -> String
toString (SteamId steamId) = steamId

exactLength :: Int
exactLength = 17

isSteamIdValid :: String -> Boolean
isSteamIdValid steamId = length steamId == exactLength && (toCharArray steamId # all isNumber)

validateSteamId :: forall errors. Maybe String -> ValidatedVariants (steamId :: String | errors) (Maybe SteamId)
validateSteamId steamId =
    validateContact steamId isSteamIdValid SteamId (SProxy :: _ "steamId") ("Invalid SteamId: " <> _)
