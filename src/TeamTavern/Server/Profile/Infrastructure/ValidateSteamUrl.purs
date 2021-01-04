module TeamTavern.Server.Profile.Infrastructure.ValidateSteamUrl where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, validateUrl)

validateSteamUrl :: String -> Either (Array String) Url
validateSteamUrl steamUrl =
    validateUrl "steamcommunity.com" steamUrl
    # lmap \errors -> [ "Invalid Steam profile URL: " <> show errors ]
