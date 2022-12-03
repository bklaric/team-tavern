module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async, alwaysRight)
import Async (Async, examineLeftWithEffect)
import Data.MultiMap (MultiMap, singleton')
import Data.Variant (inj)
import Data.Variant (inj, match)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Perun.Response (Response, badRequest, badRequest__, forbidden__, internalServerError__, noContent)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Session.StartSession as SessionStart
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CheckPassword (checkPassword)
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import TeamTavern.Server.Session.Start.LogError (StartError)
import TeamTavern.Server.Session.Start.LogError (logError)
import TeamTavern.Server.Session.Start.ReadModel (readModel)
import Type.Proxy (Proxy(..))
import Type.Proxy (Proxy(..))
import Yoga.JSON (writeJSON)

-- errorResponse :: StartError -> Response
-- errorResponse = match
--     { internal: const internalServerError__
--     , signedIn: const forbidden__
--     , unreadableDto: const badRequest__
--     , bcrypt: const internalServerError__
--     , randomError: const internalServerError__
--     , databaseError: const internalServerError__
--     , unreadableHash: const internalServerError__
--     , noMatchingPlayer: const $ badRequest
--         (singleton' "Content-Type" "application/json") $ writeJSON
--         (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)
--     , passwordDoesntMatch: const $ badRequest
--         (singleton' "Content-Type" "application/json") $ writeJSON
--         (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)
--     , noSessionStarted: const $ badRequest
--         (singleton' "Content-Type" "application/json") $ writeJSON
--         (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)
--     }

-- errorResponse :: StartError -> Response
errorResponse = match
    { internal: const $ inj (Proxy :: _ "internal") { headers: (mempty :: MultiMap String String), body: unit }
    , signedIn: const $ inj (Proxy :: _ "forbidden") { headers: (mempty :: MultiMap String String), body: unit }
    , unreadableDto: const $ inj (Proxy :: _ "internal") { headers: (mempty :: MultiMap String String), body: unit }
    , bcrypt: const $ inj (Proxy :: _ "internal") { headers: (mempty :: MultiMap String String), body: unit }
    , randomError: const $ inj (Proxy :: _ "internal") { headers: (mempty :: MultiMap String String), body: unit }
    , databaseError: const $ inj (Proxy :: _ "internal") { headers: (mempty :: MultiMap String String), body: unit }
    , unreadableHash: const $ inj (Proxy :: _ "internal") { headers: (mempty :: MultiMap String String), body: unit }
    , noMatchingPlayer: const $ inj (Proxy :: _ "badRequest") { headers: (mempty :: MultiMap String String),
        body: (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)}

    , passwordDoesntMatch: const $ inj (Proxy :: _ "badRequest") { headers: (mempty :: MultiMap String String),
        body: (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)}

    , noSessionStarted: const $ inj (Proxy :: _ "badRequest") { headers: (mempty :: MultiMap String String),
        body: (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)}
    }

-- successResponse :: Deployment -> CookieInfo -> Response
-- successResponse deployment cookieInfo = noContent $ setCookieHeaderFull deployment cookieInfo

-- successResponse :: Deployment -> CookieInfo -> Response
successResponse deployment cookieInfo = inj (Proxy :: _ "noContent")
    { headers: setCookieHeaderFull deployment cookieInfo, body: unit }

-- sendResponse :: Deployment -> Async StartError CookieInfo -> (forall left. Async left Response)
sendResponse deployment = alwaysRight errorResponse $ successResponse deployment

-- start :: forall left. Deployment -> Pool -> Cookies -> SessionStart.RequestContent -> Async left Response
start deployment pool cookies body =
    sendResponse deployment $ examineLeftWithEffect logError do
    -- Ensure player isn't signed in.
    ensureNotSignedIn cookies

    -- Read start model.
    -- model <- readModel body

    pool # withTransaction (inj (Proxy :: _ "databaseError")) \client -> do
        -- Check if password hash matches.
        { id, nickname } <- checkPassword body client

        -- Generate session token.
        token <- Token.generate

        -- Create a new session.
        createSession { id, token } client

        pure { id, nickname, token }
