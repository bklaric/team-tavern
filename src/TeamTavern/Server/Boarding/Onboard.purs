module TeamTavern.Server.Boarding.Onboard (onboard) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async as Async
import AsyncV as AsyncV
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor.Label (label)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (inj, match, onMatch)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, internalServerError__, noContent_, ok_)
import Postgres.Pool (Pool)
import Simple.JSON (writeJSON)
import TeamTavern.Routes.Onboard (BadContent, RequestContent, OkContent)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile as AddTeamProfile
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as TeamProfile
import TeamTavern.Server.Profile.Infrastructure.ConvertFields (convertFields)
import TeamTavern.Server.Team.Create.AddTeam (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (validateTeamV)

-- errorResponse :: RegisterError -> Response
errorResponse = onMatch
    { invalidBody: \errors ->
        errors
        # fromFoldable
        <#> match
            { player: inj (SProxy :: SProxy "player") <<< Array.fromFoldable
            , team: inj (SProxy :: SProxy "team") <<< Array.fromFoldable
            , playerProfile: inj (SProxy :: SProxy "playerProfile") <<< Array.fromFoldable
            , teamProfile: inj (SProxy :: SProxy "teamProfile") <<< Array.fromFoldable
            }
        # (writeJSON :: BadContent -> String)
        # badRequest_
    }
    (const internalServerError__)

-- successResponse :: SendResponseModel -> Response
successResponse = maybe noContent_ (ok_ <<< (writeJSON :: OkContent -> String)) -- const noContent_

-- sendResponse ::
--     Async RegisterError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse

onboard :: Pool -> Cookies -> Body -> Async _ Response
onboard pool cookies body =
    -- sendResponse $ examineLeftWithEffect logError do
    sendResponse $ examineLeftWithEffect (unsafeStringify >>> log) do

    -- Ensure the player is signed in.
    cookieInfo <- ensureSignedIn pool cookies

    -- Read data from body.
    (content :: RequestContent) <- readJsonBody body

    -- Start the transaction.
    pool # transaction \client -> do
        -- Read fields from database.
        fields <- loadFields client content.gameHandle

        case content of
            { ilk: 1, player: Just player, playerProfile: Just profile } -> do
                { player', profile' } <-
                    { player': _, profile': _ }
                    <$> validatePlayerV player
                    <*> validateProfileV fields profile
                    # AsyncV.toAsync
                    # label (SProxy :: SProxy "invalidBody")
                updateDetails client (unwrap cookieInfo.id) player'
                addProfile client (unwrap cookieInfo.id)
                    { handle: content.gameHandle
                    , nickname: unwrap cookieInfo.nickname
                    }
                    profile'
                pure Nothing
            { ilk: 2, team: Just team, teamProfile: Just profile } -> do
                { team', profile' } <-
                    { team': _, profile': _ }
                    <$> validateTeamV team
                    <*> TeamProfile.validateProfileV (convertFields fields) profile
                    # AsyncV.toAsync
                    # label (SProxy :: SProxy "invalidBody")
                let generatedHandle = generateHandle team'.name
                { handle } <- addTeam client cookieInfo.id generatedHandle team'
                AddTeamProfile.addProfile
                    client cookieInfo.id handle content.gameHandle profile'
                pure $ Just { teamHandle: handle }
            _ -> Async.left $ inj (SProxy :: SProxy "client") []
