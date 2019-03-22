module TeamTavern.Game.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async as Async
import Data.Either (hush)
import Data.Map (Map)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.View.LoadGame (loadGame)
import TeamTavern.Game.View.LogError (logError)
import TeamTavern.Game.View.SendResponse (sendResponse)
import TeamTavern.Infrastructure.ReadCookieInfo (readCookieInfo)

handleView :: forall left.
    Pool -> Handle -> Map String String -> Async left Response
handleView pool handle cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Attempt to read player info from cookies.
    cookieInfo <- readCookieInfo cookies # Async.attempt <#> hush

    -- Load game from database.
    loadGame pool handle cookieInfo
