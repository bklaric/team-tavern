module TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, toString, validateSteamId) where

import Prelude

import Data.Char.Unicode (isNumber)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.String (length, trim)
import Data.String.CodeUnits (toCharArray)

newtype SteamId = SteamId String

toString :: SteamId -> String
toString (SteamId steamId) = steamId

exactLength :: Int
exactLength = 17

validateSteamId :: String -> Either (Array String) SteamId
validateSteamId steamId' | steamId <- trim steamId' =
    if length steamId == exactLength && (toCharArray steamId # all isNumber)
    then Right $ SteamId steamId
    else Left [ "Invalid Steam ID: " <> steamId]
