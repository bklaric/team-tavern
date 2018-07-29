module TeamTavern.Player.Session.Start.Run (handleStart) where

import Prelude

import Async (Async)
import Data.Map (Map)
import Data.String.NonEmpty (NonEmptyString)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Run (interpret)
import Run as VariantF
import TeamTavern.Architecture.Async (examineErrorWith)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF(..))
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (ensureNotSignedIn)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)
import TeamTavern.Player.Session.Start (StartF(..), start)
import TeamTavern.Player.Session.Start.ConsumeToken (consumeToken)
import TeamTavern.Player.Session.Start.ReadNicknamedNonce (readNicknamedNonce)
import TeamTavern.Player.Session.Start.Run.CreateResponse (startResponse)
import TeamTavern.Player.Session.Start.Run.LogError (logStartError)
import TeamTavern.Player.Session.Start.Run.Types (StartError)

interpretStart
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> Async StartError { id :: PlayerId, token :: Token }
interpretStart pool nickname cookies body = start # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , start: case _ of
        ReadNicknamedNonce send ->
            readNicknamedNonce nickname body <#> send
        ConsumeToken nonce send ->
            consumeToken pool nonce <#> send
    })

handleStart
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleStart pool nickname cookies body =
    interpretStart pool nickname cookies body
    # examineErrorWith logStartError
    # startResponse
