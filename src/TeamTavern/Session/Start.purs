module TeamTavern.Session.Start where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Session.Start.ConsumeToken (consumeToken)
import TeamTavern.Session.Start.LogError (logError)
import TeamTavern.Session.Start.ReadNicknamedNonce (readNicknamedNonce)
import TeamTavern.Session.Start.Response (response)

start :: forall left. Pool -> Body -> Async left Response
start pool body =
    response $ examineLeftWithEffect logError do
    nicknamedNonce <- readNicknamedNonce body
    consumeToken pool nicknamedNonce
