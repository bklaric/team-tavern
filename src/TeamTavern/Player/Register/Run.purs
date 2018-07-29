module TeamTavern.Player.Register.Run (handleRegister) where

import Prelude

import Async (Async, fromEffect)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Console (log)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Run (interpret)
import Run as VariantF
import TeamTavern.Architecture.Async (examineErrorWith)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF(..))
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (ensureNotSignedIn)
import TeamTavern.Player.Domain.Types (Credentials)
import TeamTavern.Player.Register (RegisterF(..), register)
import TeamTavern.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Player.Register.GenerateSecrets (generateSecrets)
import TeamTavern.Player.Register.NotifyPlayer (sendRegistrationEmail)
import TeamTavern.Player.Register.ReadIdentifiers (readIdentifiers)
import TeamTavern.Player.Register.Run.CreateResponse (registerResponse)
import TeamTavern.Player.Register.Run.LogError (logError)
import TeamTavern.Player.Register.Run.Types (RegisterError)
import TeamTavern.Player.Register.ValidateIdentifiers (validateIdentifiers)

interpretRegister
    :: Pool
    -> Maybe Client
    -> Map String String
    -> Body
    -> Async RegisterError Credentials
interpretRegister pool client cookies body =
    register # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , register: case _ of
        ReadIdentifiers sendModel ->
            readIdentifiers body <#> sendModel
        ValidateIdentifiers model sendIdentifiers ->
            validateIdentifiers model <#> sendIdentifiers
        GenerateSecrets identifiers sendToken ->
            generateSecrets identifiers <#> sendToken
        AddPlayer credentials send ->
            addPlayer pool credentials <#> const send
        NotifyPlayer identifiers send ->
            case client of
            Just client' ->
                sendRegistrationEmail client' identifiers <#> const send
            Nothing ->
                "Sent registration email *wink wink* to "
                <> unwrap identifiers.nickname <> ", "
                <> unwrap identifiers.email
                <> " with nonce " <> unwrap identifiers.nonce
                # log
                # fromEffect
                <#> const send
    })

handleRegister
    :: Pool
    -> Maybe Client
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleRegister pool client cookies body =
    interpretRegister pool client cookies body
    # examineErrorWith logError
    # registerResponse
