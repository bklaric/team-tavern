module TeamTavern.Server.Team.Create (OkContent, BadContent, create) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Array as Array
import Data.Variant (Variant, match)
import Effect (Effect)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, ok_, unauthorized__)
import Postgres.Pool (Pool)
import Yoga.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, notAuthenticatedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Team.Create.AddTeam (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.LogError (teamHandler)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (TeamError, TeamErrors, validateTeam)

type CreateError = Variant
    ( internal :: Array String
    , notAuthenticated :: Array String
    , client :: Array String
    , team :: TeamErrors
    )

logError :: CreateError -> Effect Unit
logError = Log.logError "Error creating team"
    (internalHandler >>> notAuthenticatedHandler >>> clientHandler >>> teamHandler)

type OkContent = { id :: Int, handle :: String }

type BadContent = Array TeamError

sendResponse :: Async CreateError OkContent -> (forall voidLeft. Async voidLeft Response)
sendResponse = alwaysRight
    (match
        { internal: const internalServerError__
        , notAuthenticated: const unauthorized__
        , client: const badRequest__
        , team: badRequest_ <<< writeJSON <<< Array.fromFoldable
        }
    )
    (ok_ <<< writeJSON)

create :: forall left. Pool -> Body -> Cookies -> Async left Response
create pool body cookies =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- ensureSignedIn pool cookies
    content <- readJsonBody body
    team <- validateTeam content
    let generatedHandle = generateHandle team.organization cookieInfo.nickname
    addTeam pool cookieInfo.id generatedHandle team
