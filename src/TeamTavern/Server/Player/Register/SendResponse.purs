module TeamTavern.Server.Player.Register.SendResponse where

-- import Prelude

-- import Async (Async, alwaysRight)
-- import Data.Array (fromFoldable)
-- import Data.Variant (inj, match)
-- import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent)
-- import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
-- import TeamTavern.Server.Infrastructure.Deployment (Deployment)
-- import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, setCookieHeaderFull)
-- import TeamTavern.Server.Player.Register.LogError (RegisterError)
-- import Type.Proxy (Proxy(..))
-- import Yoga.JSON (writeJSON)

-- errorResponse :: RegisterError -> Response
-- errorResponse = match
--     { internal: const internalServerError__
--     , signedIn: const forbidden__
--     , unreadableDto: const badRequest__
--     , registration: \errors ->
--         errors
--         <#> (match
--             { nickname: const $ inj (Proxy :: _ "invalidNickname") {}
--             , password: const $ inj (Proxy :: _ "invalidPassword") {}
--             })
--         # fromFoldable
--         # inj (Proxy :: _ "registration")
--         # (writeJSON :: RegisterPlayer.BadContent -> String)
--         # badRequest_
--     , randomError: const internalServerError__
--     , nicknameTaken: const $ badRequest_ $ writeJSON $
--         (inj (Proxy :: _ "nicknameTaken") {} :: RegisterPlayer.BadContent)
--     , databaseError: const internalServerError__
--     , cantReadId: const internalServerError__
--     , noSessionStarted: const internalServerError__
--     }

-- successResponse :: Deployment -> CookieInfo -> Response
-- successResponse deployment cookieInfo = noContent $ setCookieHeaderFull deployment cookieInfo

-- sendResponse :: Deployment -> Async RegisterError CookieInfo -> (forall left. Async left Response)
-- sendResponse deployment = alwaysRight errorResponse $ successResponse deployment
