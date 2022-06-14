module TeamTavern.Server.Alert.Create (createAlert) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Nullable (toNullable)
import Type.Proxy (Proxy(..))
import Data.Variant (Variant, inj, match)
import Effect (Effect)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import Yoga.JSON (writeJSON)
import TeamTavern.Routes.CreateAlert as CreateAlert
import TeamTavern.Routes.Shared.Organization as Organization
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.Size as Size
import TeamTavern.Server.Infrastructure.GenerateHexString (ByteCount(..), generateHexString')
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logLines)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Infrastructure.ValidateEmail (Email, validateEmail')
import TeamTavern.Server.Infrastructure.ValidateEmail as Email

queryString :: Query
queryString = Query """
    insert into alert
        ( game_id
        , token
        , player_or_team
        , email
        , timezone
        , organizations
        , age_from
        , age_to
        , languages
        , locations
        , weekday_from
        , weekday_to
        , weekend_from
        , weekend_to
        , microphone
        , sizes
        , platforms
        , fields
        , new_or_returning
        )
    values
        ( (select id from game where handle = $1)
        , $2
        , $3
        , $4
        , $5
        , $6
        , $7
        , $8
        , $9
        , $10
        , $11
        , $12
        , $13
        , $14
        , $15
        , $16
        , $17
        , $18
        , $19
        )
    """

queryParameters :: Email -> String -> CreateAlert.RequestContent -> Array QueryParameter
queryParameters email token { handle, playerOrTeam, timezone, filters }
    = handle
    : token
    : CreateAlert.toString playerOrTeam
    : Email.toString email
    : timezone
    : (Organization.toString <$> filters.organizations)
    : toNullable filters.ageFrom
    : toNullable filters.ageTo
    : filters.languages
    : filters.locations
    : toNullable filters.weekdayFrom
    : toNullable filters.weekdayTo
    : toNullable filters.weekendFrom
    : toNullable filters.weekendTo
    : filters.microphone
    : (Size.toString <$> filters.sizes)
    : (Platform.toString <$> filters.platforms)
    : filters.fields
    :| filters.newOrReturning

type CreateAlertError = Variant
    ( client :: Array String
    , internal :: Array String
    , email :: Array String
    )

invalidEmailHandler :: forall fields. Lacks "email" fields =>
    Builder (Record fields) { email :: Array String -> Effect Unit | fields }
invalidEmailHandler = Builder.insert (Proxy :: _ "email") logLines

logError :: CreateAlertError -> Effect Unit
logError = Log.logError "Error creating alert"
    (   internalHandler
    >>> clientHandler
    >>> invalidEmailHandler
    )

errorResponse :: CreateAlertError -> Response
errorResponse = match
    { email:
        inj (Proxy :: _ "email")
        >>> (writeJSON :: CreateAlert.BadContent -> String)
        >>> badRequest_
    , client: const badRequest__
    , internal: const internalServerError__
    }

sendResponse :: Async CreateAlertError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse (const noContent_)

createAlert :: forall left. Pool -> Body -> Async left Response
createAlert pool body =
    sendResponse $ examineLeftWithEffect logError do

    -- Read data from body.
    (content :: CreateAlert.RequestContent) <- readJsonBody body

    -- Validate alert.
    email <- validateEmail' content.email

    -- Generate alert token.
    token <- generateHexString' $ ByteCount 10

    -- Write alert into database.
    queryNone pool queryString $ queryParameters email token content
