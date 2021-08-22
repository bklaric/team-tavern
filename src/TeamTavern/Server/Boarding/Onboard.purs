module TeamTavern.Server.Boarding.Onboard (onboard) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async as Async
import AsyncV as AsyncV
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor.Label (label)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Effect (Effect, foreachE)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, ok_, unauthorized__)
import Postgres.Pool (Pool)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Extra (pick)
import Simple.JSON (writeJSON)
import TeamTavern.Routes.Onboard (BadContent, RequestContent, OkContent)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logt, notAuthenticatedHandler, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as Player
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile as PlayerProfile
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile as AddTeamProfile
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as Team
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as TeamProfile
import TeamTavern.Server.Profile.Infrastructure.CheckPlayerAlerts (checkPlayerAlerts)
import TeamTavern.Server.Profile.Infrastructure.CheckTeamAlerts (checkTeamAlerts)
import TeamTavern.Server.Team.Create.AddTeam (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (TeamErrors, validateTeamV)
import Type (type ($))

type OnboardError = Variant
    ( client :: Array String
    , internal :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , invalidBody :: NonEmptyList $ Variant
        ( team :: TeamErrors
        , playerProfile :: PlayerProfile.ProfileErrors
        , teamProfile :: TeamProfile.ProfileErrors
        )
    )

invalidBodyHandler :: forall fields. Lacks "invalidBody" fields =>
    Builder (Record fields)
    { invalidBody ::
        NonEmptyList $ Variant
        ( team :: TeamErrors
        , playerProfile :: PlayerProfile.ProfileErrors
        , teamProfile :: TeamProfile.ProfileErrors
        )
        -> Effect Unit
    | fields }
invalidBodyHandler = Builder.insert (SProxy :: SProxy "invalidBody") \errors ->
    foreachE (Array.fromFoldable errors) $ match
    { team: \errors' -> logt $ "Team errors: " <> show errors'
    , playerProfile: \errors' -> logt $ "Player profile errors: " <> show errors'
    , teamProfile: \errors' -> logt $ "Team profile errors: " <> show errors'
    }

logError :: OnboardError -> Effect Unit
logError = Log.logError "Error onboarding"
    ( internalHandler
    >>> clientHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    >>> invalidBodyHandler
    )

errorResponse :: OnboardError -> Response
errorResponse = match
    { invalidBody: \errors ->
        errors
        # fromFoldable
        <#> match
            { team: inj (SProxy :: SProxy "team") <<< Array.fromFoldable
            , playerProfile: inj (SProxy :: SProxy "playerProfile") <<< Array.fromFoldable
            , teamProfile: inj (SProxy :: SProxy "teamProfile") <<< Array.fromFoldable
            }
        # (writeJSON :: BadContent -> String)
        # badRequest_
    , client: const badRequest__
    , internal: const internalServerError__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    }

successResponse :: OkContent -> Response
successResponse = ok_ <<< (writeJSON :: OkContent -> String)

sendResponse :: Async OnboardError OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse

onboard :: forall left. Pool -> Cookies -> Body -> Async left Response
onboard pool cookies body =
    sendResponse $ examineLeftWithEffect logError do

    -- Ensure the player is signed in.
    cookieInfo <- ensureSignedIn pool cookies

    -- Read data from body.
    (content :: RequestContent) <- readJsonBody body

    -- Start the transaction.
    result <- pool # transaction \client ->
        case content of
        { ilk: 1, player: Just player, playerProfile: Just profile } -> do
            -- Read fields from database.
            game <- Player.loadFields client content.gameHandle

            { player', profile' } <-
                { player': _, profile': _ }
                <$> validatePlayerV player
                <*> validateProfileV game profile
                # AsyncV.toAsync
                # label (SProxy :: SProxy "invalidBody")
            updateDetails client (unwrap cookieInfo.id) player'
            profileId <- addProfile client (unwrap cookieInfo.id)
                { handle: content.gameHandle
                , nickname: unwrap cookieInfo.nickname
                }
                profile'
            pure { teamHandle: Nothing, profileId }
        { ilk: 2, team: Just team, teamProfile: Just profile } -> do
            -- Read fields from database.
            game <- Team.loadFields client content.gameHandle

            { team', profile' } <-
                { team': _, profile': _ }
                <$> validateTeamV team
                <*> TeamProfile.validateProfileV game profile
                # AsyncV.toAsync
                # label (SProxy :: SProxy "invalidBody")
            let generatedHandle = generateHandle team'.organization cookieInfo.nickname
            { handle } <- addTeam client cookieInfo.id generatedHandle team'
            profileId <- AddTeamProfile.addProfile
                client cookieInfo.id handle content.gameHandle profile'
            pure { teamHandle: Just handle, profileId }
        _ -> Async.left $ inj (SProxy :: SProxy "client") []

    case result.teamHandle of
        Nothing -> checkPlayerAlerts result.profileId pool
        Just _ -> checkTeamAlerts result.profileId pool

    pure $ pick result
