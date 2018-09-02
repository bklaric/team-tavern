module TeamTavern.Game.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async (fromEither, left, note) as Async
import Async.Validated (fromValidated) as Async
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (labelMap)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), inj)
import Node.Errors.Class (code)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Query (query)
import Postgres.Error (constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import Simple.JSON.Async (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Game.Domain.Types (Details)
import TeamTavern.Game.Update.LogError (logError) as Update
import TeamTavern.Game.Update.Response (response) as Update
import TeamTavern.Game.Update.Types (UpdateError)
import TeamTavern.Infrastructure.Cookie (lookupAuthCookies)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Player.Domain.Types (IdentifiedToken')

readTargetHandle :: String -> Async UpdateError Handle
readTargetHandle handle =
    handle
    # Handle.create'
    # labelMap (SProxy :: SProxy "invalidTargetHandle") { handle, errors: _ }
    # Async.fromEither

readRequestor :: Map String String -> Async UpdateError IdentifiedToken'
readRequestor cookies = lookupAuthCookies cookies
    # Async.note (inj (SProxy :: SProxy "cookiesNotPresent") cookies)

type DetailsModel =
    { name :: String
    , handle :: String
    , description :: String
    }

readUpdate :: Body -> Async UpdateError Details
readUpdate body = do
    content <- readBody body
    { name, handle, description } :: DetailsModel <-
        readJSON content # labelMap
        (SProxy :: SProxy "unreadableDetailsModel") { content, errors: _ }
    { name: _, handle: _, description: _ }
        <$> (Name.create name # Validated.label (SProxy :: SProxy "name"))
        <*> (Handle.create handle # Validated.label (SProxy :: SProxy "handle"))
        <*> (Description.create description # Validated.label
            (SProxy :: SProxy "description"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidDetails")
            { name, handle, description, errors: _ }

updateGameQuery :: Query
updateGameQuery = Query """
    update game
    set name = $4, handle = $5, description = $6
    from session
    where game.administrator_id = $1
    and game.handle = $3
    and session.player_id = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    """

updateGameQueryParameters ::
    IdentifiedToken' -> Handle -> Details -> Array QueryParameter
updateGameQueryParameters
    { id, token } targetHandle { name, handle, description } =
    [ toString id
    , unwrap token
    , unwrap targetHandle
    , unwrap name
    , unwrap handle
    , unwrap description
    ]
    <#> QueryParameter

updateGame
    :: Pool
    -> IdentifiedToken'
    -> Handle
    -> Details
    -> Async UpdateError Unit
updateGame pool identifiedToken' targetHandle details = do
    result <- pool
        # query updateGameQuery
            (updateGameQueryParameters identifiedToken' targetHandle details)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "game_name_key" ->
                inj (SProxy :: SProxy "nameTaken")
                { name: details.name, error }
            true | constraint error == Just "game_handle_key" ->
                inj (SProxy :: SProxy "handleTaken")
                { handle: details.handle, error }
            _ -> inj (SProxy :: SProxy "databaseError") error)
    if rowCount result == 1
        then pure unit
        else Async.left $ inj (SProxy :: SProxy "notAuthorized")
            { id: identifiedToken'.id
            , token: identifiedToken'.token
            , handle: targetHandle
            }

handleUpdate :: forall left.
    Pool -> String -> Map String String -> Body -> Async left Response
handleUpdate pool targetHandle' cookies body =
    Update.response $ examineLeftWithEffect Update.logError do
    -- Read target handle from route.
    targetHandle <- readTargetHandle targetHandle'

    -- Read requestor authentication info from cookie.
    identifiedToken <- readRequestor cookies

    -- Read update from body.
    details <- readUpdate body

    -- Update game.
    updateGame pool identifiedToken targetHandle details
