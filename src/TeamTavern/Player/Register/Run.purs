module TeamTavern.Player.Register.Run where

import Prelude

import Async (Async(Async), fromEffect)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (ExceptT(..))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Run (interpret, match)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Register (RegisterF(..), register)
import TeamTavern.Player.Register.Database (addPlayer)
import TeamTavern.Player.Register.Email (sendRegistrationEmail)
import TeamTavern.Player.Register.Errors (RegisterError, fromRegisterPlayerErrors)
import TeamTavern.Player.Register.Identifiers (readIdentifiers, validateIdentifiers)
import TeamTavern.Player.Register.Token (generateToken)
import Unsafe.Coerce (unsafeCoerce)

interpretRegister ::
    Pool -> Maybe Client -> Body -> Async RegisterError Credentials
interpretRegister pool client body = register # interpret (match
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
            case client of
            Just client' ->
                sendRegistrationEmail client' credentials <#> const send
            Nothing ->
                "Sent email *wink wink* to " <> unwrap credentials.email
                # unsafeCoerce log # fromEffect <#> const send
    })

anyLeft
    :: forall inLeft inRight outRight
    .  (inLeft -> outRight)
    -> (inRight -> outRight)
    -> Either inLeft inRight
    -> (forall voidLeft. Either voidLeft outRight)
anyLeft leftFunction rightFunction either' =
    Right $ either leftFunction rightFunction either'

anyAsyncLeft
    :: forall inLeft inRight outRight
    .  (inLeft -> outRight)
    -> (inRight -> outRight)
    -> Async inLeft inRight
    -> (forall voidLeft. Async voidLeft outRight)
anyAsyncLeft leftFunction rightFunction (Async (ExceptT eitherCont)) =
    eitherCont <#> anyLeft leftFunction rightFunction # ExceptT # Async

respondRegister ::
    Async RegisterError Credentials -> (forall left. Async left Response)
respondRegister registerAsync = registerAsync # anyAsyncLeft
    (\error ->
        { statusCode: 400
        , content: error # fromRegisterPlayerErrors # writeJSON
        })
    (\player ->
        { statusCode: 200
        , content: "Looks good: "
            <> unwrap player.email <> ", "
            <> unwrap player.nickname
        })

handleRegister ::
    Pool -> Maybe Client -> Body -> (forall left. Async left Response)
handleRegister pool client body =
    interpretRegister pool client body # respondRegister
