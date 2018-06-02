module TeamTavern.Player.Register.Run where

import Prelude

import Async (Async, fromEffect, runAsync)
import Control.Monad.Eff.Console (log)
import Data.Either (either)
import Data.Newtype (unwrap)
import Effect (Effect)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Run (interpret, match)
import Simple.JSON (writeJSON)
import TeamTavern.Architecture.Environment (Environment(..))
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Register (RegisterF(..), register)
import TeamTavern.Player.Register.Database (addPlayer)
import TeamTavern.Player.Register.Email (sendRegistrationEmail)
import TeamTavern.Player.Register.Errors (RegisterError, fromRegisterPlayerErrors)
import TeamTavern.Player.Register.Identifiers (readIdentifiers, validateIdentifiers)
import TeamTavern.Player.Register.Token (generateToken)
import Unsafe.Coerce (unsafeCoerce)

interpretRegister ::
    Environment -> Pool -> Client -> Body -> Async RegisterError Credentials
interpretRegister environment pool client body = register # interpret (match
    { register: case _ of
        ReadIdentifiers sendModel ->
            readIdentifiers body <#> sendModel
        ValidateIdentifiers model sendIdentifiers ->
            validateIdentifiers model <#> sendIdentifiers
        GenerateToken sendToken ->
            generateToken <#> sendToken
        AddPlayer credentials send -> do
            addPlayer pool credentials <#> const send
        SendEmail credentials send ->
            case environment of
            Deployed -> sendRegistrationEmail client credentials <#> const send
            Local -> "Sent email *wink wink* to " <> unwrap credentials.email
                # log # unsafeCoerce # fromEffect <#> const send
    })

respondRegister ::
    (Response -> Effect Unit) -> Async RegisterError Credentials -> Effect Unit
respondRegister respond registerAsync = runAsync registerAsync $
    either
        (\error -> respond
            { statusCode: 400
            , content: error # fromRegisterPlayerErrors # writeJSON
            })
        (\player -> respond
            { statusCode: 200
            , content: "Looks good: "
                <> unwrap player.email <> ", "
                <> unwrap player.nickname
            })

handleRegister
    :: Environment
    -> Pool
    -> Client
    -> Body
    -> (Response -> Effect Unit)
    -> Effect Unit
handleRegister environment pool client body respond =
    interpretRegister environment pool client body
    # respondRegister respond
