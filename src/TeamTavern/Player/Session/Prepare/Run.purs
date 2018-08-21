module TeamTavern.Player.Session.Prepare.Run (handlePrepare) where

import Prelude

import Async (Async, examineLeftWithEffect, fromEffect)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Effect.Console (log)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Run (interpret)
import Run as VariantF
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF(..))
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (ensureNotSignedIn)
import TeamTavern.Player.Infrastructure.ValidateIdentifiers (validateIdentifiers)
import TeamTavern.Player.Session.Prepare (PrepareF(..), prepare)
import TeamTavern.Player.Session.Prepare.CreateSession (createSession)
import TeamTavern.Player.Session.Prepare.GenerateSecrets (generateSecrets)
import TeamTavern.Player.Session.Prepare.NotifyPlayer (notifyPlayer)
import TeamTavern.Player.Session.Prepare.ReadIdentifiers (readIdentifiers)
import TeamTavern.Player.Session.Prepare.Run.CreateResponse (prepareResponse)
import TeamTavern.Player.Session.Prepare.Run.LogError (logError)
import TeamTavern.Player.Session.Prepare.Run.Types (PrepareError)

interpretPrepare
    :: Pool
    -> Maybe Client
    -> NonEmptyString
    -> Body
    -> Map String String
    -> Async PrepareError Unit
interpretPrepare pool client nickname' body cookies =
    prepare # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , prepare: case _ of
        ReadIdentifiers send ->
            readIdentifiers nickname' body <#> send
        ValidateIdentifiers identifiers send ->
            validateIdentifiers identifiers <#> send
        GenerateSecrets email nickname send ->
            generateSecrets email nickname <#> send
        CreateSession credentials send ->
            createSession pool credentials <#> const send
        NotifyPlayer identifiers send ->
            case client of
            Just client' -> notifyPlayer client' identifiers <#> const send
            Nothing ->
                "Sent start session email *wink wink* to "
                <> unwrap identifiers.nickname <> ", "
                <> unwrap identifiers.email
                <> " with nonce " <> unwrap identifiers.nonce
                # log
                # fromEffect
                <#> const send
    })

handlePrepare
    :: Pool
    -> Maybe Client
    -> NonEmptyString
    -> Body
    -> Map String String
    -> (forall left. Async left Response)
handlePrepare pool client nickname body cookies =
    interpretPrepare pool client nickname body cookies
    # examineLeftWithEffect logError
    # prepareResponse
