module TeamTavern.Server.Infrastructure.FetchSteamNickname (SummariesContent, fetchSteamNickname) where

import Prelude

import Async (Async, attempt)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import TeamTavern.Server.Infrastructure.Log (logStamped)
import TeamTavern.Server.Infrastructure.ResolveSteamId (SteamApi, getSteamApi)
import TeamTavern.Server.Player.Domain.Nickname (nicknameOf)

-- | What GetPlayerSummaries answers, as far as the site reads it.
type SummariesContent = { response :: { players :: Array { steamid :: String, personaname :: String } } }

fetchPersonaName :: SteamApi -> String -> Async String (Maybe String)
fetchPersonaName steamApi steamId = do
    { content } :: { content :: SummariesContent, body :: String } <-
        getSteamApi steamApi "/ISteamUser/GetPlayerSummaries/v2/" (Object.singleton "steamids" steamId)
    pure $ content.response.players # find (_.steamid >>> eq steamId) <#> _.personaname

-- | The Steam account's profile name as a nickname, which the nickname prompt
-- | offers. Steam failing to answer leaves the prompt empty rather than the
-- | sign-in failed, so the failure is logged here.
fetchSteamNickname :: ∀ left. SteamApi -> String -> Async left String
fetchSteamNickname steamApi steamId =
    fetchPersonaName steamApi steamId # attempt >>= case _ of
        Right personaName -> pure $ maybe "" nicknameOf personaName
        Left line -> do
            liftEffect $ logStamped $ "Error fetching the Steam profile name | SteamID64: " <> steamId <> " | " <> line
            pure ""
