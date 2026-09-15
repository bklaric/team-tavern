module TeamTavern.Client.Script.Discord (authorizeWithDiscord) where

import Prelude

import Data.Maybe (fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import JSURI (encodeURIComponent)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

-- | Sends the browser to Discord, which sends it back to the page at `path` on
-- | the current origin with an access token in the fragment. The origin, scheme
-- | included, has to be one of the redirect URIs registered on the Discord app.
authorizeWithDiscord :: ∀ effect. MonadEffect effect => String -> effect Unit
authorizeWithDiscord path = do
    origin' <- window >>= location >>= origin # liftEffect
    hardNavigate $ "https://discord.com/api/oauth2/authorize"
        <> "?client_id=1068667687661740052"
        <> "&redirect_uri=" <> (encodeURIComponent (origin' <> path) # fromMaybe "")
        <> "&response_type=token"
        <> "&scope=identify%20email"
        <> "&prompt=none"
