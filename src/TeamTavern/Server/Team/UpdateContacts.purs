module TeamTavern.Server.Team.UpdateContacts (updateContacts) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Array as Array
import Type.Proxy (Proxy(..))
import Data.Variant (Variant, match)
import Effect (Effect, foreachE)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:))
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import Yoga.JSON (writeJSON)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInOwner (ensureSignedInOwner)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logt, notAuthenticatedHandler, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (queryMany, transaction)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts (ContactsErrors, validateContacts)
import TeamTavern.Server.Team.Infrastructure.WriteContacts (writeContacts)

-- Load plaftorms

queryString :: Query
queryString = Query $ """
    select distinct unnest(team_profile.platforms) as platform
    from team_profile
    where team_profile.team_id = $1;
    """

loadRequiredPlatforms :: forall querier errors. Querier querier =>
    querier -> Int -> Async (InternalError errors) (Array Platform)
loadRequiredPlatforms querier id =
    (queryMany querier queryString (id : []) :: Async _ (Array { platform :: Platform }))
    <#> map _.platform

-- Log
type UpdateContactsError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , teamContacts :: ContactsErrors
    )

contactsHandler :: forall fields. Lacks "teamContacts" fields =>
    Builder (Record fields) { teamContacts :: ContactsErrors -> Effect Unit | fields }
contactsHandler = Builder.insert (Proxy :: _ "teamContacts") \errors ->
    foreachE (Array.fromFoldable errors) $ match
    { discordTag: logt
    , discordServer: logt
    , steamId: logt
    , riotId: logt
    , battleTag: logt
    , eaId: logt
    , psnId: logt
    , gamerTag: logt
    , friendCode: logt
    }

logError :: UpdateContactsError -> Effect Unit
logError = Log.logError "Error updating team contacts"
    ( internalHandler
    >>> clientHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    >>> contactsHandler
    )

-- Response
errorResponse :: UpdateContactsError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    , teamContacts: Array.fromFoldable >>> writeJSON >>> badRequest_
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse :: Async UpdateContactsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse

-- Main
updateContacts :: forall left.
    Pool -> Body -> Cookies -> { handle :: String } -> Async left Response
updateContacts pool body cookies { handle } =
    sendResponse $ examineLeftWithEffect logError do

    -- Read contacts from body.
    contacts' <- readJsonBody body

    pool # transaction \client -> do
        -- Read requestor info from cookies.
        { teamId } <- ensureSignedInOwner client cookies handle

        -- Read required platforms.
        requiredPlatforms <- loadRequiredPlatforms client teamId

        -- Validate contacts.
        contacts <- validateContacts requiredPlatforms contacts'

        -- Update contacts.
        writeContacts client teamId contacts
