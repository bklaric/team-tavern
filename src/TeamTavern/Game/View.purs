module TeamTavern.Game.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async as Async
import Data.Either (hush)
import Data.Map (Map)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Infrastructure.ReadHandle (readHandle)
import TeamTavern.Game.View.LoadGame (loadGame)
import TeamTavern.Game.View.LogError (logError)
import TeamTavern.Game.View.Response (response)
import TeamTavern.Infrastructure.ReadAuth (readAuth)

handleView :: forall left.
    Pool -> String -> Map String String -> Async left Response
handleView pool handle' cookies =
    response $ examineLeftWithEffect logError do
    -- Validate game handle from route.
    handle <- readHandle handle'

    -- Attempt to read auth info from cookies.
    auth <- readAuth cookies # Async.attempt <#> hush

    -- Load game from database.
    loadGame pool handle auth
