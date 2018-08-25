module TeamTavern.Player.View.Run where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.String.NonEmpty (NonEmptyString)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Run (interpret, match)
import TeamTaver.Player.View.Run.LogError (logError)
import TeamTavern.Player.Infrastructure (readNickname)
import TeamTavern.Player.View (ViewF(..), PlayerView, view)
import TeamTavern.Player.View.LoadPlayer (loadPlayer)
import TeamTavern.Player.View.Run.CreateResponse (viewResponse)
import TeamTavern.Player.View.Run.Types (ViewError)

interpretView :: Pool -> NonEmptyString -> Async ViewError PlayerView
interpretView pool nickname' =
    view # interpret (match
    { view: case _ of
        ReadNickname send -> readNickname nickname' <#> send
        LoadPlayer nickname send -> loadPlayer pool nickname <#> send
    })

handleView :: Pool -> NonEmptyString -> (forall left. Async left Response)
handleView pool nickname =
    interpretView pool nickname
    # examineLeftWithEffect logError
    # viewResponse
