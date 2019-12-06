module TeamTavern.Server.Player.ViewAccount where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.ViewAccount.LoadAccount (loadAccount)
import TeamTavern.Server.Player.ViewAccount.LogError (logError)
import TeamTavern.Server.Player.ViewAccount.SendResponse (sendResponse)

viewAccount :: forall left. Pool -> Nickname -> Cookies -> Async left Response
viewAccount pool nickname cookies =
    sendResponse $ examineLeftWithEffect logError do

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Ensure player is signed in as requested nickname.
            cookieInfo <- ensureSignedInAs client cookies (unwrap nickname)

            -- Load account.
            loadAccount client (unwrap nickname)
