module TeamTavern.Player.Session.Prepare.Run (handlePrepare) where

import Prelude

import Async (Async, examineLeftWithEffect, fromEffect)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Effect.Console (log)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Run (interpret)
import Run as VariantF
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF(..))
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (ensureNotSignedIn)
import TeamTavern.Player.Session.Prepare (PrepareF(..), prepare)
import TeamTavern.Player.Session.Prepare.CreateSession (createSession)
import TeamTavern.Player.Session.Prepare.GenerateSecrets (generateSecrets)
import TeamTavern.Player.Session.Prepare.NotifyPlayer (notifyPlayer)
import TeamTavern.Player.Session.Prepare.ReadNickname (readNickname)
import TeamTavern.Player.Session.Prepare.Run.CreateResponse (prepareResponse)
import TeamTavern.Player.Session.Prepare.Run.LogError (logError)
import TeamTavern.Player.Session.Prepare.Run.Types (PrepareError)

interpretPrepare
    :: Pool
    -> Maybe Client
    -> NonEmptyString
    -> Map String String
    -> Async PrepareError Unit
interpretPrepare pool client nickname' cookies =
    prepare # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , prepare: case _ of
        ReadNickname send ->
            readNickname nickname' <#> send
        GenerateSecrets nickname send ->
            generateSecrets nickname <#> send
        CreateSession secrets send ->
            createSession pool secrets <#> send
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
    -> Map String String
    -> (forall left. Async left Response)
handlePrepare pool client nickname cookies =
    interpretPrepare pool client nickname cookies
    # examineLeftWithEffect logError
    # prepareResponse
